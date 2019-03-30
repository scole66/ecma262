# 8.1 Lexical Environments
#
# A Lexical Environment is a specification type used to define the association of Identifiers to specific variables and
# functions based upon the lexical nesting structure of ECMAScript code. A Lexical Environment consists of an
# Environment Record and a possibly null reference to an outer Lexical Environment. Usually a Lexical Environment is
# associated with some specific syntactic structure of ECMAScript code such as a FunctionDeclaration, a BlockStatement,
# or a Catch clause of a TryStatement and a new Lexical Environment is created each time such code is evaluated.
#
# An Environment Record records the identifier bindings that are created within the scope of its associated Lexical
# Environment. It is referred to as the Lexical Environment's EnvironmentRecord.
#
# The outer environment reference is used to model the logical nesting of Lexical Environment values. The outer
# reference of a (inner) Lexical Environment is a reference to the Lexical Environment that logically surrounds the
# inner Lexical Environment. An outer Lexical Environment may, of course, have its own outer Lexical Environment. A
# Lexical Environment may serve as the outer environment for multiple inner Lexical Environments. For example, if a
# FunctionDeclaration contains two nested FunctionDeclarations then the Lexical Environments of each of the nested
# functions will have as their outer Lexical Environment the Lexical Environment of the current evaluation of the
# surrounding function.
#
# A global environment is a Lexical Environment which does not have an outer environment. The global environment's
# outer environment reference is null. A global environment's EnvironmentRecord may be prepopulated with identifier
# bindings and includes an associated global object whose properties provide some of the global environment's
# identifier bindings. As ECMAScript code is executed, additional properties may be added to the global object and the
# initial properties may be modified.
#
# A module environment is a Lexical Environment that contains the bindings for the top level declarations of a Module.
# It also contains the bindings that are explicitly imported by the Module. The outer environment of a module
# environment is a global environment.
#
# A function environment is a Lexical Environment that corresponds to the invocation of an ECMAScript function object.
# A function environment may establish a new this binding. A function environment also captures the state necessary to
# support super method invocations.
#
# Lexical Environments and Environment Record values are purely specification mechanisms and need not correspond to any
# specific artefact of an ECMAScript implementation. It is impossible for an ECMAScript program to directly access or
# manipulate such values.

from collections import namedtuple
from completion_record import NormalCompletion, ThrowCompletion, Empty

#
# 8.1.1 Environment Records
#
# There are two primary kinds of Environment Record values used in this specification: declarative Environment Records
# and object Environment Records. Declarative Environment Records are used to define the effect of ECMAScript language
# syntactic elements such as FunctionDeclarations, VariableDeclarations, and Catch clauses that directly associate
# identifier bindings with ECMAScript language values. Object Environment Records are used to define the effect of
# ECMAScript elements such as WithStatement that associate identifier bindings with the properties of some object.
# Global Environment Records and function Environment Records are specializations that are used for specifically for
# Script global declarations and for top-level declarations within functions.
#
# For specification purposes Environment Record values are values of the Record specification type and can be thought
# of as existing in a simple object-oriented hierarchy where Environment Record is an abstract class with three
# concrete subclasses, declarative Environment Record, object Environment Record, and global Environment Record.
# Function Environment Records and module Environment Records are subclasses of declarative Environment Record. The
# abstract class includes the abstract specification methods defined in Table 14. These abstract methods have distinct
# concrete algorithms for each of the concrete subclasses.
#
# Table 14: Abstract Methods of Environment Records
#
# +------------------------------+-------------------------------------------------------------------------------------
# | Method                       | Purpose
# +------------------------------+-------------------------------------------------------------------------------------
# | HasBinding(N)                | Determine if an Environment Record has a binding for the String value N. Return true
# |                              | if it does and false if it does not.
# +------------------------------+-------------------------------------------------------------------------------------
# | CreateMutableBinding(N, D)   | Create a new but uninitialized mutable binding in an Environment Record. The String
# |                              | value N is the text of the bound name. If the Boolean argument D is true the binding
# |                              | may be subsequently deleted.
# +------------------------------+-------------------------------------------------------------------------------------
# | CreateImmutableBinding(N, S) | Create a new but uninitialized immutable binding in an Environment Record. The
# |                              | String value N is the text of the bound name. If S is true then attempts to set it
# |                              | after it has been initialized will always throw an exception, regardless of the
# |                              | strict mode setting of operations that reference that binding.
# +------------------------------+-------------------------------------------------------------------------------------
# | InitializeBinding(N, V)      | Set the value of an already existing but uninitialized binding in an Environment
# |                              | Record. The String value N is the text of the bound name. V is the value for the
# |                              | binding and is a value of any ECMAScript language type.
# +------------------------------+-------------------------------------------------------------------------------------
# | SetMutableBinding(N, V, S)   | Set the value of an already existing mutable binding in an Environment Record. The
# |                              | String value N is the text of the bound name. V is the value for the binding and may
# |                              | be a value of any ECMAScript language type. S is a Boolean flag. If S is true and
# |                              | the binding cannot be set throw a TypeError exception.
# +------------------------------+-------------------------------------------------------------------------------------
# | GetBindingValue(N, S)        | Returns the value of an already existing binding from an Environment Record. The
# |                              | String value N is the text of the bound name. S is used to identify references
# |                              | originating in strict mode code or that otherwise require strict mode reference
# |                              | semantics. If S is true and the binding does not exist throw a ReferenceError
# |                              | exception. If the binding exists but is uninitialized a ReferenceError is thrown,
# |                              | regardless of the value of S.
# +------------------------------+-------------------------------------------------------------------------------------
# | DeleteBinding(N)             | Delete a binding from an Environment Record. The String value N is the text of the
# |                              | bound name. If a binding for N exists, remove the binding and return true. If the
# |                              | binding exists but cannot be removed return false. If the binding does not exist
# |                              | return true.
# +------------------------------+-------------------------------------------------------------------------------------
# | HasThisBinding()             | Determine if an Environment Record establishes a this binding. Return true if it
# |                              | does and false if it does not.
# +------------------------------+-------------------------------------------------------------------------------------
# | HasSuperBinding()            | Determine if an Environment Record establishes a super method binding. Return true
# |                              | if it does and false if it does not.
# +------------------------------+-------------------------------------------------------------------------------------
# | WithBaseObject()             | If this Environment Record is associated with a with statement, return the with
# |                              | object. Otherwise, return undefined.
# +------------------------------+-------------------------------------------------------------------------------------

# 8.1.1.1 Declarative Environment Records
#
# Each declarative Environment Record is associated with an ECMAScript program scope containing variable, constant,
# let, class, module, import, and/or function declarations. A declarative Environment Record binds the set of
# identifiers defined by the declarations contained within its scope.
#
class DeclarativeEnvironmentRecord:
    Binding = namedtuple('Binding', ['value', 'mutable', 'strict', 'deletable', 'initialized'])
    def __init__(self):
        self.bindings = {}

    # 8.1.1.1.1 HasBinding ( N )
    def HasBinding(self, N):
        """Determine if an Environment Record has a binding for the String value N. Return true if it does and false if
           it does not."""
        # The concrete Environment Record method HasBinding for declarative Environment Records simply determines if
        # the argument identifier is one of the identifiers bound by the record:
        # 1. Let envRec be the declarative Environment Record for which the method was invoked.
        # 2. If envRec has a binding for the name that is the value of N, return true.
        # 3. Return false.
        return N in self.bindings

    # 8.1.1.1.2 CreateMutableBinding ( N, D )
    def CreateMutableBinding(self, N, D):
        """Create a new but uninitialized mutable binding in an Environment Record. The String value N is the text of
           the bound name. If the Boolean argument D is true the binding may be subsequently deleted."""
        # The concrete Environment Record method CreateMutableBinding for declarative Environment Records creates a new
        # mutable binding for the name N that is uninitialized. A binding must not already exist in this Environment
        # Record for N. If Boolean argument D has the value true the new binding is marked as being subject to deletion.
        # 1. Let envRec be the declarative Environment Record for which the method was invoked.
        # 2. Assert: envRec does not already have a binding for N.
        assert N not in self.bindings
        # 3. Create a mutable binding in envRec for N and record that it is uninitialized. If D is true, record that
        #    the newly created binding may be deleted by a subsequent DeleteBinding call.
        self.bindings[N] = self.Binding(None, True, False, D, False)
        # 4. Return NormalCompletion(empty).
        return NormalCompletion(Empty.EMPTY)

    # 8.1.1.1.3 CreateImmutableBinding ( N, S )
    def CreateImmutableBinding(self, N, S):
        """Create a new but uninitialized immutable binding in an Environment Record. The String value N is the text of
           the bound name. If S is true then attempts to set it after it has been initialized will always throw an
           exception, regardless of the strict mode setting of operations that reference that binding."""
        # The concrete Environment Record method CreateImmutableBinding for declarative Environment Records creates a
        # new immutable binding for the name N that is uninitialized. A binding must not already exist in this
        # Environment Record for N. If the Boolean argument S has the value true the new binding is marked as a strict
        # binding.
        # 1. Let envRec be the declarative Environment Record for which the method was invoked.
        # 2. Assert: envRec does not already have a binding for N.
        assert N not in self.bindings
        # 3. Create an immutable binding in envRec for N and record that it is uninitialized. If S is true, record that
        #    the newly created binding is a strict binding.
        self.bindings[N] = self.Binding(None, False, S, False, False)
        # 4. Return NormalCompletion(empty).
        return NormalCompletion(Empty.EMPTY)

    # 8.1.1.1.4 InitializeBinding ( N, V )
    def InitializeBinding(self, N, V):
        """Set the value of an already existing but uninitialized binding in an Environment
           Record. The String value N is the text of the bound name. V is the value for the
           binding and is a value of any ECMAScript language type."""
        # The concrete Environment Record method InitializeBinding for declarative Environment Records is used to set
        # the bound value of the current binding of the identifier whose name is the value of the argument N to the
        # value of argument V. An uninitialized binding for N must already exist.
        # 1. Let envRec be the declarative Environment Record for which the method was invoked.
        # 2. Assert: envRec must have an uninitialized binding for N.
        assert N in self.bindings and not self.bindings[N].initialized
        # 3. Set the bound value for N in envRec to V.
        # 4. Record that the binding for N in envRec has been initialized.
        self.bindings[N] = self.bindings[N]._replace(value=V, initialized=True)
        # 5. Return NormalCompletion(empty).
        return NormalCompletion(Empty.EMPTY)

    # 8.1.1.1.5 SetMutableBinding ( N, V, S )
    def SetMutableBinding(self, N, V, S):
        """Set the value of an already existing mutable binding in an Environment Record. The
        String value N is the text of the bound name. V is the value for the binding and may
        be a value of any ECMAScript language type. S is a Boolean flag. If S is true and
        the binding cannot be set throw a TypeError exception."""
        # The concrete Environment Record method SetMutableBinding for declarative Environment Records attempts to
        # change the bound value of the current binding of the identifier whose name is the value of the argument N to
        # the value of argument V. A binding for N normally already exists, but in rare cases it may not. If the
        # binding is an immutable binding, a TypeError is thrown if S is true.
        # 1. Let envRec be the declarative Environment Record for which the method was invoked.
        # 2. If envRec does not have a binding for N, then
        if N not in self.bindings:
            # a. If S is true, throw a ReferenceError exception.
            if S:
                return ThrowCompletion(ReferenceError())
            # b. Perform envRec.CreateMutableBinding(N, true).
            self.CreateMutableBinding(N, True)
            # c. Perform envRec.InitializeBinding(N, V).
            self.InitializeBinding(N, V)
            # d. Return NormalCompletion(empty).
            return NormalCompletion(Empty.EMPTY)
        # 3. If the binding for N in envRec is a strict binding, set S to true.
        if self.bindings[N].strict:
            S = True
        # 4. If the binding for N in envRec has not yet been initialized, throw a ReferenceError exception.
        if not self.bindings[N].initialized:
            return ThrowCompletion(ReferenceError())
        # 5. Else if the binding for N in envRec is a mutable binding, change its bound value to V.
        if self.bindings[N].mutable:
            self.bindings[N] = self.bindings[N]._replace(value=V)
        # 6. Else,
        else:
            # a. Assert: This is an attempt to change the value of an immutable binding.
            # b. If S is true, throw a TypeError exception.
            if S:
                return ThrowCompletion(TypeError())
        # 7. Return NormalCompletion(empty).
        return NormalCompletion(Empty.EMPTY)
        # NOTE
        # An example of ECMAScript code that results in a missing binding at step 2 is:
        #
        # function f(){eval("var x; x = (delete x, 0);")}

    # 8.1.1.1.6 GetBindingValue ( N, S )
    def GetBindingValue(self, N, _):
        # The concrete Environment Record method GetBindingValue for declarative Environment Records simply returns
        # the value of its bound identifier whose name is the value of the argument N. If the binding exists but is
        # uninitialized a ReferenceError is thrown, regardless of the value of S.
        # 1. Let envRec be the declarative Environment Record for which the method was invoked.
        # 2. Assert: envRec has a binding for N.
        # 3. If the binding for N in envRec is an uninitialized binding, throw a ReferenceError exception.
        if not self.bindings[N].initialized:
            return ThrowCompletion(ReferenceError())
        # 4. Return the value currently bound to N in envRec.
        return NormalCompletion(self.bindings[N].value)

    # 8.1.1.1.7 DeleteBinding ( N )
    def DeleteBinding(self, N):
        # The concrete Environment Record method DeleteBinding for declarative Environment Records can only delete
        # bindings that have been explicitly designated as being subject to deletion.
        # 1. Let envRec be the declarative Environment Record for which the method was invoked.
        # 2. Assert: envRec has a binding for the name that is the value of N.
        # 3. If the binding for N in envRec cannot be deleted, return false.
        if not self.bindings[N].deletable:
            return NormalCompletion(False)
        # 4. Remove the binding for N from envRec.
        del self.bindings[N]
        # 5. Return true.
        return NormalCompletion(True)

    # 8.1.1.1.8 HasThisBinding ( )
    def HasThisBinding(self):
        # Regular declarative Environment Records do not provide a this binding.
        #
        # 1. Return false.
        return False

    # 8.1.1.1.9 HasSuperBinding ( )
    def HasSuperBinding(self):
        # Regular declarative Environment Records do not provide a super binding.
        #
        # 1. Return false.
        return False

    # 8.1.1.1.10 WithBaseObject ( )
    def WithBaseObject(self):
        # Declarative Environment Records always return undefined as their WithBaseObject.
        #
        # 1. Return undefined.
        return None

# 8.1.1.2 Object Environment Records
#
# Each object Environment Record is associated with an object called its binding object. An object Environment Record
# binds the set of string identifier names that directly correspond to the property names of its binding object.
# Property keys that are not strings in the form of an IdentifierName are not included in the set of bound identifiers.
# Both own and inherited properties are included in the set regardless of the setting of their [[Enumerable]]
# attribute. Because properties can be dynamically added and deleted from objects, the set of identifiers bound by an
# object Environment Record may potentially change as a side-effect of any operation that adds or deletes properties.
# Any bindings that are created as a result of such a side-effect are considered to be a mutable binding even if the
# Writable attribute of the corresponding property has the value false. Immutable bindings do not exist for object
# Environment Records.
#
# Object Environment Records created for with statements (13.11) can provide their binding object as an implicit this
# value for use in function calls. The capability is controlled by a withEnvironment Boolean value that is associated
# with each object Environment Record. By default, the value of withEnvironment is false for any object Environment
# Record.

# 8.1.1.4 Global Environment Records
#
# A global Environment Record is used to represent the outer most scope that is shared by all of the ECMAScript Script
# elements that are processed in a common realm. A global Environment Record provides the bindings for built-in globals
# (clause 18), properties of the global object, and for all top-level declarations (13.2.8, 13.2.10) that occur within
# a Script.
#
# A global Environment Record is logically a single record but it is specified as a composite encapsulating an object
# Environment Record and a declarative Environment Record. The object Environment Record has as its base object the
# global object of the associated Realm Record. This global object is the value returned by the global Environment
# Record's GetThisBinding concrete method. The object Environment Record component of a global Environment Record
# contains the bindings for all built-in globals (clause 18) and all bindings introduced by a FunctionDeclaration,
# GeneratorDeclaration, AsyncFunctionDeclaration, AsyncGeneratorDeclaration, or VariableStatement contained in global
# code. The bindings for all other ECMAScript declarations in global code are contained in the declarative Environment
# Record component of the global Environment Record.
#
# Properties may be created directly on a global object. Hence, the object Environment Record component of a global
# Environment Record may contain both bindings created explicitly by FunctionDeclaration, GeneratorDeclaration,
# AsyncFunctionDeclaration, AsyncGeneratorDeclaration, or VariableDeclaration declarations and bindings created
# implicitly as properties of the global object. In order to identify which bindings were explicitly created using
# declarations, a global Environment Record maintains a list of the names bound using its CreateGlobalVarBinding and
# CreateGlobalFunctionBinding concrete methods.
#
# Global Environment Records have the additional fields listed in Table 17 and the additional methods listed in Table
# 18.
#
# Table 17: Additional Fields of Global Environment Records
# +-----------------------+--------------------------------+------------------------------------------------------------
# | Field Name            | Value                          | Meaning
# +-----------------------+--------------------------------+------------------------------------------------------------
# | [[ObjectRecord]]      | Object Environment Record      | Binding object is the global object. It contains global
# |                       |                                | built-in bindings as well as FunctionDeclaration,
# |                       |                                | GeneratorDeclaration, AsyncFunctionDeclaration,
# |                       |                                | AsyncGeneratorDeclaration, and VariableDeclaration bindings
# |                       |                                | in global code for the associated realm.
# +-----------------------+--------------------------------+------------------------------------------------------------
# | [[GlobalThisValue]]   | Object                         | The value returned by this in global scope. Hosts may
# |                       |                                | provide any ECMAScript Object value.
# +-----------------------+--------------------------------+------------------------------------------------------------
# | [[DeclarativeRecord]] | Declarative Environment Record | Contains bindings for all declarations in global code for
# |                       |                                | the associated realm code except for FunctionDeclaration,
# |                       |                                | GeneratorDeclaration, AsyncFunctionDeclaration,
# |                       |                                | AsyncGeneratorDeclaration, and VariableDeclaration
# |                       |                                | bindings.
# +-----------------------+--------------------------------+------------------------------------------------------------
# | [[VarNames]]          | List of String                 | The string names bound by FunctionDeclaration,
# |                       |                                | GeneratorDeclaration, AsyncFunctionDeclaration,
# |                       |                                | AsyncGeneratorDeclaration, and VariableDeclaration
# |                       |                                | declarations in global code for the associated realm.
# +-----------------------+--------------------------------+------------------------------------------------------------
#
# Table 18: Additional Methods of Global Environment Records
# +--------------------------------------+------------------------------------------------------------------------------
# | Method                               | Purpose
# +--------------------------------------+------------------------------------------------------------------------------
# | GetThisBinding()                     | Return the value of this Environment Record's this binding.
# +--------------------------------------+------------------------------------------------------------------------------
# | HasVarDeclaration (N)                | Determines if the argument identifier has a binding in this Environment
# |                                      | Record that was created using a VariableDeclaration, FunctionDeclaration,
# |                                      | GeneratorDeclaration, AsyncFunctionDeclaration, or AsyncGeneratorDeclaration.
# +--------------------------------------+------------------------------------------------------------------------------
# | HasLexicalDeclaration (N)            | Determines if the argument identifier has a binding in this Environment
# |                                      | Record that was created using a lexical declaration such as a
# |                                      | LexicalDeclaration or a ClassDeclaration.
# +--------------------------------------+------------------------------------------------------------------------------
# | HasRestrictedGlobalProperty (N)      | Determines if the argument is the name of a global object property that may
# |                                      | not be shadowed by a global lexical binding.
# +--------------------------------------+------------------------------------------------------------------------------
# | CanDeclareGlobalVar (N)              | Determines if a corresponding CreateGlobalVarBinding call would succeed if
# |                                      | called for the same argument N.
# +--------------------------------------+------------------------------------------------------------------------------
# | CanDeclareGlobalFunction (N)         | Determines if a corresponding CreateGlobalFunctionBinding call would succeed
# |                                      | if called for the same argument N.
# +--------------------------------------+------------------------------------------------------------------------------
# | CreateGlobalVarBinding(N, D)         | Used to create and initialize to undefined a global var binding in the
# |                                      | [[ObjectRecord]] component of a global Environment Record. The binding will
# |                                      | be a mutable binding. The corresponding global object property will have
# |                                      | attribute values appropriate for a var. The String value N is the bound name.
# |                                      | If D is true the binding may be deleted. Logically equivalent to
# |                                      | CreateMutableBinding followed by a SetMutableBinding but it allows var
# |                                      | declarations to receive special treatment.
# +--------------------------------------+------------------------------------------------------------------------------
# | CreateGlobalFunctionBinding(N, V, D) | Create and initialize a global function binding in the [[ObjectRecord]]
# |                                      | component of a global Environment Record. The binding will be a mutable
# |                                      | binding. The corresponding global object property will have attribute values
# |                                      | appropriate for a function. The String value N is the bound name. V is the
# |                                      | initialization value. If the Boolean argument D is true the binding may be
# |                                      | deleted. Logically equivalent to CreateMutableBinding followed by a
# |                                      | SetMutableBinding but it allows function declarations to receive special
# |                                      | treatment.
# +--------------------------------------+------------------------------------------------------------------------------

