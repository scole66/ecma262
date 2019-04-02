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
from errors import CreateReferenceError, CreateTypeError

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
                return ThrowCompletion(CreateReferenceError())
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
            return ThrowCompletion(CreateReferenceError())
        # 5. Else if the binding for N in envRec is a mutable binding, change its bound value to V.
        if self.bindings[N].mutable:
            self.bindings[N] = self.bindings[N]._replace(value=V)
        # 6. Else,
        else:
            # a. Assert: This is an attempt to change the value of an immutable binding.
            # b. If S is true, throw a TypeError exception.
            if S:
                return ThrowCompletion(CreateTypeError())
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
            return ThrowCompletion(CreateReferenceError())
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

class ObjectEnvironmentRecord:
    def __init__(self, binding_object, with_environment):
        self.binding_object = ToObject(binding_object)
        self.with_environment = ToBoolean(with_environment)

    # 8.1.1.2.1 HasBinding ( N )
    def HasBinding(self, N):
        # The concrete Environment Record method HasBinding for object Environment Records determines if its associated
        # binding object has a property whose name is the value of the argument N:
        # 1. Let envRec be the object Environment Record for which the method was invoked.
        # 2. Let bindings be the binding object for envRec.
        # 3. Let foundBinding be ? HasProperty(bindings, N).
        cr = HasProperty(self.binding_object, N)
        if cr.ctype != CompletionType.NORMAL:
            return cr
        found_binding = cr.value
        # 4. If foundBinding is false, return false.
        if not found_binding:
            return NormalCompletion(False)
        # 5. If the withEnvironment flag of envRec is false, return true.
        if not with_environment:
            return NormalCompletion(True)
        # 6. Let unscopables be ? Get(bindings, @@unscopables).
        cr = Get(self.binding_object, wks_unscopables)
        if cr.ctype != CompletionType.NORMAL:
            return cr
        unscopables = cr.value
        # 7. If Type(unscopables) is Object, then
        if isObject(unscopables):
            # a. Let blocked be ToBoolean(? Get(unscopables, N)).
            cr = Get(unscopables, N)
            if cr.ctype != CompletionType.NORMAL:
                return cr
            blocked = ToBoolean(cr.value)
            # b. If blocked is true, return false.
            if blocked:
                return NormalCompletion(False)
        # 8. Return true.
        return NormalCompletion(True)

    # 8.1.1.2.2 CreateMutableBinding ( N, D )
    def CreateMutableBinding(self, name, deletable):
        # The concrete Environment Record method CreateMutableBinding for object Environment Records creates in an
        # Environment Record's associated binding object a property whose name is the String value and initializes it
        # to the value undefined. If Boolean argument D has the value true the new property's [[Configurable]]
        # attribute is set to true; otherwise it is set to false.

        # 1. Let envRec be the object Environment Record for which the method was invoked.
        # 2. Let bindings be the binding object for envRec.
        # 3. Return ? DefinePropertyOrThrow(bindings, N, PropertyDescriptor { [[Value]]: undefined, [[Writable]]: true,
        #             [[Enumerable]]: true, [[Configurable]]: D }).
        return DefinePropertyOrThrow(self.binding_object, name,
                                     PropertyDescriptor(Value=None, Writable=True,
                                                        Enumerable=True, Configurable=deletable))
        # NOTE
        # Normally envRec will not have a binding for N but if it does, the semantics of DefinePropertyOrThrow may
        # result in an existing binding being replaced or shadowed or cause an abrupt completion to be returned.

    # 8.1.1.2.4 InitializeBinding ( N, V )
    def InitializeBinding(self, name, value):
        # The concrete Environment Record method InitializeBinding for object Environment Records is used to set the
        # bound value of the current binding of the identifier whose name is the value of the argument N to the value of
        # argument V. An uninitialized binding for N must already exist.

        # 1. Let envRec be the object Environment Record for which the method was invoked.
        # 2. Assert: envRec must have an uninitialized binding for N.
        # 3. Record that the binding for N in envRec has been initialized.
        # 4. Return ? envRec.SetMutableBinding(N, V, false).
        return self.SetMutableBinding(name, value, False)
        # NOTE
        # In this specification, all uses of CreateMutableBinding for object Environment Records are immediately
        # followed by a call to InitializeBinding for the same name. Hence, implementations do not need to explicitly
        # track the initialization state of individual object Environment Record bindings.

    # 8.1.1.2.5 SetMutableBinding ( N, V, S )
    def SetMutableBinding(self, name, value, strict):
        # The concrete Environment Record method SetMutableBinding for object Environment Records attempts to set the
        # value of the Environment Record's associated binding object's property whose name is the value of the
        # argument N to the value of argument V. A property named N normally already exists but if it does not or is
        # not currently writable, error handling is determined by the value of the Boolean argument S.

        # 1. Let envRec be the object Environment Record for which the method was invoked.
        # 2. Let bindings be the binding object for envRec.
        # 3. Return ? Set(bindings, N, V, S).
        return Set(self.binding_object, name, value, strict)

    # 8.1.1.2.6 GetBindingValue ( N, S )
    def GetBindingValue(self, name, strict):
        # The concrete Environment Record method GetBindingValue for object Environment Records returns the value of
        # its associated binding object's property whose name is the String value of the argument identifier N. The
        # property should already exist but if it does not the result depends upon the value of the S argument:

        # 1. Let envRec be the object Environment Record for which the method was invoked.
        # 2. Let bindings be the binding object for envRec.
        # 3. Let value be ? HasProperty(bindings, N).
        value, ok = ec(HasProperty(self.binding_object, name))
        if not ok:
            return value
        # 4. If value is false, then
        if not value:
            # a. If S is false, return the value undefined; otherwise throw a ReferenceError exception.
            if not strict:
                return NormalCompletion(None)
            return ThrowCompletion(CreateReferenceError())
        # 5. Return ? Get(bindings, N).
        return Get(self.binding_object, name)

    # 8.1.1.2.7 DeleteBinding ( N )
    def DeleteBinding(self, name):
        # The concrete Environment Record method DeleteBinding for object Environment Records can only delete bindings
        # that correspond to properties of the environment object whose [[Configurable]] attribute have the value true.

        # 1. Let envRec be the object Environment Record for which the method was invoked.
        # 2. Let bindings be the binding object for envRec.
        # 3. Return ? bindings.[[Delete]](N).
        return self.binding_object.Delete(name)

    # 8.1.1.2.8 HasThisBinding ( )
    def HasThisBinding(self):
        # Regular object Environment Records do not provide a this binding.
        # 1. Return false.
        return NormalCompletion(False)

    # 8.1.1.2.9 HasSuperBinding ( )
    def HasSuperBinding(self):
        # Regular object Environment Records do not provide a super binding.
        # 1. Return false.
        return NormalCompletion(False)

    # 8.1.1.2.10 WithBaseObject ( )
    def WithBaseObject(self):
        # Object Environment Records return undefined as their WithBaseObject unless their withEnvironment flag is
        # true.

        # 1. Let envRec be the object Environment Record for which the method was invoked.
        # 2. If the withEnvironment flag of envRec is true, return the binding object for envRec.
        if self.with_environment:
            return NormalCompletion(self.binding_object)
        # 3. Otherwise, return undefined.
        return NormalCompletion(None)


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

class GlobalEnvironmentRecord:
    def __init__(self, binding_object, global_this_value):
        self.object_record = ObjectEnvironmentRecord(binding_object, False)
        self.global_this_value = ToObject(global_this_value)
        self.declarative_record = DeclarativeEnvironmentRecord()
        self.var_names = []

    # 8.1.1.4.1 HasBinding ( N )
    def HasBinding(self, name):
        # The concrete Environment Record method HasBinding for global Environment Records simply determines if the
        # argument identifier is one of the identifiers bound by the record:
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let DclRec be envRec.[[DeclarativeRecord]].
        # 3. If DclRec.HasBinding(N) is true, return true.
        if self.declarative_record.HasBinding(name):
            return NormalCompletion(True)
        # 4. Let ObjRec be envRec.[[ObjectRecord]].
        # 5. Return ? ObjRec.HasBinding(N).
        return self.object_record.HasBinding(name)

    # 8.1.1.4.2 CreateMutableBinding ( N, D )
    def CreateMutableBinding(self, name, deletable):
        # The concrete Environment Record method CreateMutableBinding for global Environment Records creates a new
        # mutable binding for the name N that is uninitialized. The binding is created in the associated
        # DeclarativeRecord. A binding for N must not already exist in the DeclarativeRecord. If Boolean argument D has
        # the value true the new binding is marked as being subject to deletion.
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let DclRec be envRec.[[DeclarativeRecord]].
        # 3. If DclRec.HasBinding(N) is true, throw a TypeError exception.
        if self.declarative_record.HasBinding(name):
            return ThrowCompletion(CreateTypeError())
        # 4. Return DclRec.CreateMutableBinding(N, D).
        return self.declarative_record.CreateMutableBinding(name, deletable)

    # 8.1.1.4.3 CreateImmutableBinding ( N, S )
    def CreateImmutableBinding(self, name, strict):
        # The concrete Environment Record method CreateImmutableBinding for global Environment Records creates a new
        # immutable binding for the name N that is uninitialized. A binding must not already exist in this Environment
        # Record for N. If the Boolean argument S has the value true the new binding is marked as a strict binding.
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let DclRec be envRec.[[DeclarativeRecord]].
        # 3. If DclRec.HasBinding(N) is true, throw a TypeError exception.
        if self.declarative_record.HasBinding(name):
            return ThrowCompletion(CreateTypeError())
        # 4. Return DclRec.CreateImmutableBinding(N, S).
        return self.declarative_record.CreateImmutableBinding(name, strict)

    # 8.1.1.4.4 InitializeBinding ( N, V )
    def InitializeBinding(self, name, value):
        # The concrete Environment Record method InitializeBinding for global Environment Records is used to set the
        # bound value of the current binding of the identifier whose name is the value of the argument N to the value
        # of argument V. An uninitialized binding for N must already exist.
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let DclRec be envRec.[[DeclarativeRecord]].
        # 3. If DclRec.HasBinding(N) is true, then
        if self.declarative_record.HasBinding(name):
            # a. Return DclRec.InitializeBinding(N, V).
            return self.declarative_record.InitializeBinding(name, value)
        # 4. Assert: If the binding exists, it must be in the object Environment Record.
        # 5. Let ObjRec be envRec.[[ObjectRecord]].
        # 6. Return ? ObjRec.InitializeBinding(N, V).
        return self.object_record.InitializeBinding(name, value)

    # 8.1.1.4.5 SetMutableBinding ( N, V, S )
    def SetMutableBinding(self, name, value, strict):
        # The concrete Environment Record method SetMutableBinding for global Environment Records attempts to change
        # the bound value of the current binding of the identifier whose name is the value of the argument N to the
        # value of argument V. If the binding is an immutable binding, a TypeError is thrown if S is true. A property
        # named N normally already exists but if it does not or is not currently writable, error handling is determined
        # by the value of the Boolean argument S.
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let DclRec be envRec.[[DeclarativeRecord]].
        # 3. If DclRec.HasBinding(N) is true, then
        if self.declarative_record.HasBinding(name):
            # a. Return DclRec.SetMutableBinding(N, V, S).
            return self.declarative_record.SetMutableBinding(name, value, strict)
        # 4. Let ObjRec be envRec.[[ObjectRecord]].
        # 5. Return ? ObjRec.SetMutableBinding(N, V, S).
        return self.object_record.SetMutableBinding(name, value, strict)

    # 8.1.1.4.6 GetBindingValue ( N, S )
    def GetBindingValue(self, name, strict):
        # The concrete Environment Record method GetBindingValue for global Environment Records returns the value of
        # its bound identifier whose name is the value of the argument N. If the binding is an uninitialized binding
        # throw a ReferenceError exception. A property named N normally already exists but if it does not or is not
        # currently writable, error handling is determined by the value of the Boolean argument S.
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let DclRec be envRec.[[DeclarativeRecord]].
        # 3. If DclRec.HasBinding(N) is true, then
        if self.declarative_record.HasBinding(name):
            # a. Return DclRec.GetBindingValue(N, S).
            return self.declarative_record.GetBindingValue(name, strict)
        # 4. Let ObjRec be envRec.[[ObjectRecord]].
        # 5. Return ? ObjRec.GetBindingValue(N, S).
        return self.object_record.GetBindingValue(name, strict)

    # 8.1.1.4.7 DeleteBinding ( N )
    def DeleteBinding(self, name):
        # The concrete Environment Record method DeleteBinding for global Environment Records can only delete bindings
        # that have been explicitly designated as being subject to deletion.
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let DclRec be envRec.[[DeclarativeRecord]].
        # 3. If DclRec.HasBinding(N) is true, then
        if self.declarative_record.HasBinding(name):
            # a. Return DclRec.DeleteBinding(N).
            return self.declarative_record.DeleteBinding(name)
        # 4. Let ObjRec be envRec.[[ObjectRecord]].
        # 5. Let globalObject be the binding object for ObjRec.
        global_object = self.object_record.binding_object
        # 6. Let existingProp be ? HasOwnProperty(globalObject, N).
        existing_prop, ok = ec(HasOwnProperty(global_object, name))
        if not ok:
            return existing_prop
        # 7. If existingProp is true, then
        if existing_prop:
            # a. Let status be ? ObjRec.DeleteBinding(N).
            status, ok = ec(self.object_record.DeleteBinding(name))
            if not ok:
                return status
            # b. If status is true, then
            if status:
                # i. Let varNames be envRec.[[VarNames]].
                # ii. If N is an element of varNames, remove that element from the varNames.
                try:
                    self.var_names.remove(name)
                except ValueError:
                    pass
            # c. Return status.
            return NormalCompletion(status)
        # 8. Return true.
        return NormalCompletion(True)

    # 8.1.1.4.8 HasThisBinding ( )
    def HasThisBinding(self):
        # Return true.
        return NormalCompletion(True)

    # 8.1.1.4.9 HasSuperBinding ( )
    def HasSuperBinding(self):
        # Return false.
        return NormalCompletion(False)

    # 8.1.1.4.10 WithBaseObject ( )
    def WithBaseObject(self):
        # Global Environment Records always return undefined as their WithBaseObject.
        # Return undefined.
        return NormalCompletion(None)

    # 8.1.1.4.11 GetThisBinding ( )
    def GetThisBinding(self):
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Return envRec.[[GlobalThisValue]].
        return NormalCompletion(self.global_this_value)

    # 8.1.1.4.12 HasVarDeclaration ( N )
    def HasVarDeclaration(self, name):
        # The concrete Environment Record method HasVarDeclaration for global Environment Records determines if the
        # argument identifier has a binding in this record that was created using a VariableStatement or a
        # FunctionDeclaration:
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let varDeclaredNames be envRec.[[VarNames]].
        # 3. If varDeclaredNames contains N, return true.
        # 4. Return false.
        return NormalCompletion(name in self.var_names)

    # 8.1.1.4.13 HasLexicalDeclaration ( N )
    def HasLexicalDeclaration(self, name):
        # The concrete Environment Record method HasLexicalDeclaration for global Environment Records determines if the
        # argument identifier has a binding in this record that was created using a lexical declaration such as a
        # LexicalDeclaration or a ClassDeclaration:
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let DclRec be envRec.[[DeclarativeRecord]].
        # 3. Return DclRec.HasBinding(N).
        return NormalCompletion(self.declarative_record.HasBinding(name))

    # 8.1.1.4.14 HasRestrictedGlobalProperty ( N )
    def HasRestrictedGlobalProperty(self, name):
        # The concrete Environment Record method HasRestrictedGlobalProperty for global Environment Records determines
        # if the argument identifier is the name of a property of the global object that must not be shadowed by a
        # global lexical binding:
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let ObjRec be envRec.[[ObjectRecord]].
        # 3. Let globalObject be the binding object for ObjRec.
        global_object = self.object_record.binding_object
        # 4. Let existingProp be ? globalObject.[[GetOwnProperty]](N).
        existing_prop, ok = ec(global_object.GetOwnProperty(name))
        if not ok:
            return existing_prop
        # 5. If existingProp is undefined, return false.
        if isUndefined(existing_prop):
            return NormalCompletion(False)
        # 6. If existingProp.[[Configurable]] is true, return false.
        # 7. Return true.
        return NormalCompletion(not existing_prop.Configurable)
        # NOTE
        # Properties may exist upon a global object that were directly created rather than being declared using a var
        # or function declaration. A global lexical binding may not be created that has the same name as a
        # non-configurable property of the global object. The global property undefined is an example of such a
        # property.

    # 8.1.1.4.15 CanDeclareGlobalVar ( N )
    def CanDeclareGlobalVar(self, name):
        # The concrete Environment Record method CanDeclareGlobalVar for global Environment Records determines if a
        # corresponding CreateGlobalVarBinding call would succeed if called for the same argument N. Redundant var
        # declarations and var declarations for pre-existing global object properties are allowed.
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let ObjRec be envRec.[[ObjectRecord]].
        # 3. Let globalObject be the binding object for ObjRec.
        global_object = self.object_record.binding_object
        # 4. Let hasProperty be ? HasOwnProperty(globalObject, N).
        has_property, ok = ec(HasOwnProperty(global_object, name))
        if not ok:
            return has_property
        # 5. If hasProperty is true, return true.
        if has_property:
            return NormalCompletion(True)
        # 6. Return ? IsExtensible(globalObject).
        return IsExtensible(global_object)

    # 8.1.1.4.16 CanDeclareGlobalFunction ( N )
    def CanDeclareGlobalFunction(self, name):
        # The concrete Environment Record method CanDeclareGlobalFunction for global Environment Records determines if
        # a corresponding CreateGlobalFunctionBinding call would succeed if called for the same argument N.
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let ObjRec be envRec.[[ObjectRecord]].
        # 3. Let globalObject be the binding object for ObjRec.
        global_object = self.object_record.binding_object
        # 4. Let existingProp be ? globalObject.[[GetOwnProperty]](N).
        existing_prop, ok = ec(global_object.GetOwnProperty(name))
        if not ok:
            return existing_prop
        # 5. If existingProp is undefined, return ? IsExtensible(globalObject).
        if isUndefined(existing_prop):
            return IsExtensible(global_object)
        # 6. If existingProp.[[Configurable]] is true, return true.
        if existing_prop['configurable']:
            return NormalCompletion(True)
        # 7. If IsDataDescriptor(existingProp) is true and existingProp has attribute values { [[Writable]]: true,
        #    [[Enumerable]]: true }, return true.
        if IsDataDescriptor(existing_prop) and existing_prop['writable'] and existing_prop['enumerable']:
            return NormalCompletion(True)
        # 8. Return false.
        return NormalCompletion(False)

    # 8.1.1.4.17 CreateGlobalVarBinding ( N, D )
    def CreateGlobalVarBinding(self, name, deletable):
        # The concrete Environment Record method CreateGlobalVarBinding for global Environment Records creates and
        # initializes a mutable binding in the associated object Environment Record and records the bound name in the
        # associated [[VarNames]] List. If a binding already exists, it is reused and assumed to be initialized.
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let ObjRec be envRec.[[ObjectRecord]].
        # 3. Let globalObject be the binding object for ObjRec.
        global_object = self.object_record.binding_object
        # 4. Let hasProperty be ? HasOwnProperty(globalObject, N).
        has_property, ok = ec(HasOwnProperty(global_object, name))
        if not ok:
            return has_property
        # 5. Let extensible be ? IsExtensible(globalObject).
        extensible, ok = ec(IsExtensible(global_object))
        if not ok:
            return extensible
        # 6. If hasProperty is false and extensible is true, then
        if not has_property and extensible:
            # a. Perform ? ObjRec.CreateMutableBinding(N, D).
            cr, ok = ec(self.object_record.CreateMutableBinding(name, deletable))
            if not ok:
                return cr
            # b. Perform ? ObjRec.InitializeBinding(N, undefined).
            cr, ok = ec(self.object_record.InitializeBinding(name, None))
            if not ok:
                return cr
        # 7. Let varDeclaredNames be envRec.[[VarNames]].
        # 8. If varDeclaredNames does not contain N, then
        if name not in self.var_names:
            # a. Append N to varDeclaredNames.
            self.var_names.append(name)
        # 9. Return NormalCompletion(empty).
        return NormalCompletion(Empty.EMPTY)

    # 8.1.1.4.18 CreateGlobalFunctionBinding ( N, V, D )
    def CreateGlobalFunctionBinding(self, name, value, deletable):
        # The concrete Environment Record method CreateGlobalFunctionBinding for global Environment Records creates and
        # initializes a mutable binding in the associated object Environment Record and records the bound name in the
        # associated [[VarNames]] List. If a binding already exists, it is replaced.
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let ObjRec be envRec.[[ObjectRecord]].
        # 3. Let globalObject be the binding object for ObjRec.
        global_object = self.object_record.binding_object
        # 4. Let existingProp be ? globalObject.[[GetOwnProperty]](N).
        existing_prop, ok = ec(global_object.GetOwnProperty(name))
        if not ok:
            return existing_prop
        # 5. If existingProp is undefined or existingProp.[[Configurable]] is true, then
        if existing_prop is None or existing_prop['configurable']:
            # a. Let desc be the PropertyDescriptor { [[Value]]: V, [[Writable]]: true, [[Enumerable]]: true,
            #    [[Configurable]]: D }.
            desc = { 'value': value, 'writable': True, 'enumerable': True, 'configurable': deletable }
        # 6. Else,
        else:
            # a. Let desc be the PropertyDescriptor { [[Value]]: V }.
            desc = { 'value': value }
        # 7. Perform ? DefinePropertyOrThrow(globalObject, N, desc).
        cr, ok = ec(DefinePropertyOrThrow(global_object, name, desc))
        if not ok:
            return cr
        # 8. Record that the binding for N in ObjRec has been initialized.
        # .... object records aren't supposed to need to do this, so I'm not sure where I'm storing this info and when
        # it's used. A problem for later....
        pass
        # 9. Perform ? Set(globalObject, N, V, false).
        cr, ok = ec(Set(global_object, name, value, False))
        if not ok:
            return cr
        # 10. Let varDeclaredNames be envRec.[[VarNames]].
        # 11. If varDeclaredNames does not contain N, then
        if name not in self.var_names:
            # a. Append N to varDeclaredNames.
            self.var_names.append(name)
        # 12. Return NormalCompletion(empty).
        return NormalCompletion(Empty.EMPTY)
        # NOTE
        # Global function declarations are always represented as own properties of the global object. If possible, an
        # existing own property is reconfigured to have a standard set of attribute values. Steps 8-9 are equivalent to
        # what calling the InitializeBinding concrete method would do and if globalObject is a Proxy will produce the
        # same sequence of Proxy trap calls.
