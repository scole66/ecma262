"""
Chapter 8: Executable Code and Execution Contexts
"""
import uuid
from collections import deque, namedtuple
from itertools import repeat

from condition_record import Empty, NormalCompletion, ec

# 8.2 Realms
#
# Before it is evaluated, all ECMAScript code must be associated with a realm. Conceptually, a realm consists of a set
# of intrinsic objects, an ECMAScript global environment, all of the ECMAScript code that is loaded within the scope of
# that global environment, and other associated state and resources.
#
# A realm is represented in this specification as a Realm Record with the fields specified in Table 20:
#
# Table 20: Realm Record Fields
# +------------------+----------------------------------------+------------------------------------------------------
# | Field Name       | Value                                  | Meaning
# +------------------+----------------------------------------+------------------------------------------------------
# | [[Intrinsics]]   | Record whose field names are intrinsic | The intrinsic values used by code associated with
# |                  | keys and whose values are objects      | this realm
# +------------------+----------------------------------------+------------------------------------------------------
# | [[GlobalObject]] | Object                                 | The global object for this realm
# +------------------+----------------------------------------+------------------------------------------------------
# | [[GlobalEnv]]    | Lexical Environment                    | The global environment for this realm
# +------------------+----------------------------------------+------------------------------------------------------
# | [[TemplateMap]]  | A List of Record                       | Template objects are canonicalized separately for
# |                  | { [[Site]]: Parse Node,                | each realm using its Realm Record's [[TemplateMap]].
# |                  |   [[Array]]: Object }.                 | Each [[Site]] value is a Parse Node that is a
# |                  |                                        | TemplateLiteral. The associated [[Array]] value is the
# |                  |                                        | corresponding template object that is passed to a tag
# |                  |                                        | function.
# +------------------+----------------------------------------+------------------------------------------------------
# | [[HostDefined]]  |   Any, default value is undefined.     | Field reserved for use by host environments that need
# |                  |                                        | to associate additional information with a Realm
# |                  |                                        | Record.
# +------------------+----------------------------------------+------------------------------------------------------
#
# NOTE (for [[TemplateMap]])
# Once a Parse Node becomes unreachable, the corresponding [[Array]] is also unreachable, and it would be unobservable
# if an implementation removed the pair from the [[TemplateMap]] list.

class Realm:
    def __init__(self):
        self.intrinsics = {}
        self.global_object = None
        self.global_env = None
        self.template_map = []
        self.host_defined = None

# 8.2.1 CreateRealm ( )
def CreateRealm():
    # The abstract operation CreateRealm with no arguments performs the following steps:
    # 1. Let realmRec be a new Realm Record.
    realm_rec = Realm()
    # 2. Perform CreateIntrinsics(realmRec).
    CreateIntrinsics(realm_rec)
    # 3. Set realmRec.[[GlobalObject]] to undefined.
    pass  # Did this in the record creation already
    # 4. Set realmRec.[[GlobalEnv]] to undefined.
    pass  # Did this in the record creation already
    # 5. Set realmRec.[[TemplateMap]] to a new empty List.
    pass
    # 6. Return realmRec.
    return realm_rec

# 8.2.2 CreateIntrinsics ( realmRec )
def CreateIntrinsics(realm_rec):
    # The abstract operation CreateIntrinsics with argument realmRec performs the following steps:
    #
    # 1. Let intrinsics be a new Record.
    intrinsics = {}
    # 2. Set realmRec.[[Intrinsics]] to intrinsics.
    realm_rec.intrinsics = intrinsics
    # 3. Let objProto be ObjectCreate(null).
    obj_proto = ObjectCreate(None)
    # 4. Set intrinsics.[[%ObjectPrototype%]] to objProto.
    intrinsics['%ObjectPrototype%'] = obj_proto
    # 5. Let throwerSteps be the algorithm steps specified in 9.2.9.1 for the %ThrowTypeError% function.
    thrower_steps = object_behaviors.ThrowTypeError
    # 6. Let thrower be CreateBuiltinFunction(throwerSteps, « », realmRec, null).
    thrower = CreateBuiltinFunction(thrower_steps, [], self, None)
    # 7. Set intrinsics.[[%ThrowTypeError%]] to thrower.
    intrinsics['%ThrowTypeError%'] = thrower
    # 8. Let noSteps be an empty sequence of algorithm steps.
    no_steps = lambda: None
    # 9. Let funcProto be CreateBuiltinFunction(noSteps, « », realmRec, objProto).
    func_proto = CreateBuiltinFunction(no_steps, [], realm_rec, obj_proto)
    # 10. Set intrinsics.[[%FunctionPrototype%]] to funcProto.
    intrinsics['%FunctionPrototype%'] = func_proto
    # 11. Call thrower.[[SetPrototypeOf]](funcProto).
    thrower.set_prototype_of(func_proto)
    # 12. Perform AddRestrictedFunctionProperties(funcProto, realmRec).
    AddRestrictedFunctionProperties(func_proto, realm_rec)
    # 13. Set fields of intrinsics with the values listed in Table 7 that have not already been handled above. The
    #     field names are the names listed in column one of the table. The value of each field is a new object
    #     value fully and recursively populated with property values as defined by the specification of each object
    #     in clauses 18-26. All object property values are newly created object values. All values that are
    #     built-in function objects are created by performing CreateBuiltinFunction(<steps>, <slots>, realmRec,
    #     <prototype>) where <steps> is the definition of that function provided by this specification, <slots> is
    #     a list of the names, if any, of the function's specified internal slots, and <prototype> is the specified
    #     value of the function's [[Prototype]] internal slot. The creation of the intrinsics and their properties
    #     must be ordered to avoid any dependencies upon objects that have not yet been created.
    pass # yeah, later, dudez
    # 14. Return intrinsics.
    return intrinsics


# 8.5 InitializeHostDefinedRealm ( )
def InitializeHostDefinedRealm():
    # The abstract operation InitializeHostDefinedRealm performs the following steps:
    #
    # 1. Let realm be CreateRealm().
    realm = CreateRealm()
    # 2. Let newContext be a new execution context.
    new_context = ExecutionContext()
    # 3. Set the Function of newContext to null.
    new_context.ecma_function = None
    # 4. Set the Realm of newContext to realm.
    new_context.realm = realm
    # 5. Set the ScriptOrModule of newContext to null.
    new_context.script_or_module = None
    # 6. Push newContext onto the execution context stack; newContext is now the running execution context.
    surrounding_agent.ec_stack.append(new_context)
    surrounding_agent.running_ec = new_context
    # 7. If the host requires use of an exotic object to serve as realm's global object, let global be such an object
    #    created in an implementation-defined manner. Otherwise, let global be undefined, indicating that an ordinary
    #    object should be created as the global object.
    global = None
    # 8. If the host requires that the this binding in realm's global scope return an object other than the global
    #    object, let thisValue be such an object created in an implementation-defined manner. Otherwise, let thisValue
    #    be undefined, indicating that realm's global this binding should be the global object.
    this_value = None
    # 9. Perform SetRealmGlobalObject(realm, global, thisValue).
    SetRealmGlobalObject(realm, global, this_value)
    # 10. Let globalObj be ? SetDefaultGlobalBindings(realm).
    global_obj, ok = ec(SetDefaultGlobalBindings(realm))
    if not ok
        return global_obj
    # 11. Create any implementation-defined global object properties on globalObj.
    pass # Gonna want to add things like "console" here...
    # 12. Return NormalCompletion(empty).
    return NormalCompletion(Empty.EMPTY)

# 8.6 RunJobs
def RunJobs(scripts=[], modules=[]):
    # 1. Perform ? InitializeHostDefinedRealm().
    cr, ok = ec(InitializeHostDefinedRealm())
    if not ok
        return cr
    # 2. In an implementation-dependent manner, obtain the ECMAScript source texts (see clause 10) and any
    #    associated host-defined values for zero or more ECMAScript scripts and/or ECMAScript modules.
    host_defined = None
    #    For each such sourceText and hostDefined, do
    #        a. If sourceText is the source code of a script, then
    #            i. Perform EnqueueJob("ScriptJobs", ScriptEvaluationJob, « sourceText, hostDefined »).
    #        b. Else sourceText is the source code of a module,
    #            i. Perform EnqueueJob("ScriptJobs", TopLevelModuleEvaluationJob, « sourceText, hostDefined »).
    for source_text in modules:
        EnqueueJob("ScriptJobs", TopLevelModuleEvaluationJob, [source_text, host_defined])
    for source_text in scripts:
        EnqueueJob("ScriptJobs", ScriptEvaluationJob, [source_text, host_defined])
    # 3. Repeat,
    while 1:
        # a. Suspend the running execution context and remove it from the execution context stack.
        surrounding_agent.running_ec.suspend()
        surrounding_agent.running_ec = None
        surrounding_agent.ec_stack.pop() # discarding the result
        # b. Assert: The execution context stack is now empty.
        assert len(surrounding_agent.ec_stack) == 0
        # c. Let nextQueue be a non-empty Job Queue chosen in an implementation-defined manner. If all Job Queues are
        #    empty, the result is implementation-defined.
        non_empty_job_queues = [ name for name, queue in surrounding_agent.job_queues.items() if len(queue) > 0 ]
        if len(non_empty_job_queues) == 0:
            break
        next_queue = random.choice(non_empty_job_queues)
        # d. Let nextPending be the PendingJob record at the front of nextQueue. Remove that record from nextQueue.
        next_pending = surrounding_agent.job_queues[name].popleft()
        # e. Let newContext be a new execution context.
        new_context = ExecutionContext()
        # f. Set newContext's Function to null.
        new_context.ecma_function = None
        # g. Set newContext's Realm to nextPending.[[Realm]].
        new_context.realm = next_pending.realm
        # h. Set newContext's ScriptOrModule to nextPending.[[ScriptOrModule]].
        new_context.script_or_module = next_pending.script_or_module
        # i. Push newContext onto the execution context stack; newContext is now the running execution context.
        surrounding_agent.ec_stack.append(new_context)
        surrounding_agent.running_ec = new_context
        # j. Perform any implementation or host environment defined job initialization using nextPending.
        # (Nothing yet.)
        # k. Let result be the result of performing the abstract operation named by nextPending.[[Job]] using the
        #    elements of nextPending.[[Arguments]] as its arguments.
        result, ok = ec((next_pending.job)(*next_pending.arguments))
        # l. If result is an abrupt completion, perform HostReportErrors(« result.[[Value]] »).
        if not ok:
            HostReportErrors([result])

# 8.7 Agents

# An agent comprises a set of ECMAScript execution contexts, an execution context stack, a running execution context, a
# set of named job queues, an Agent Record, and an executing thread. Except for the executing thread, the constituents
# of an agent belong exclusively to that agent.

# An agent's executing thread executes the jobs in the agent's job queues on the agent's execution contexts
# independently of other agents, except that an executing thread may be used as the executing thread by multiple
# agents, provided none of the agents sharing the thread have an Agent Record whose [[CanBlock]] property is true.

# While an agent's executing thread executes the jobs in the agent's job queues, the agent is the surrounding agent
# for the code in those jobs. The code uses the surrounding agent to access the specification level execution objects
# held within the agent: the running execution context, the execution context stack, the named job queues, and the
# Agent Record's fields.
class AgentRecord(object):
    def __init__(self):
        self.little_endian = True
        self.can_block = True # as a first guess
        self.signifier = uuid.uuid4()
        self.is_lock_free_1 = True
        self.is_lock_free_2 = True

class Agent(object):
    def __init__(self):
        self.ec_stack = [] # For this stack, add with "append", remove with "pop". (LIFO)
        self.running_ec = None
        self.job_queues = {  # For these queues, add with "append", remove with "popleft". (FIFO)
            'ScriptJobs': deque([])
            'PromiseJobs': deque([])
        }
        self.agent_record = AgentRecord()

# Global: the "surrounding agent". (We only have one agent, so it's always the surrounding agent.)
surrounding_agent = Agent()

# 8.7.1 AgentSignifier()
def AgentSignifier():
    # The abstract operation AgentSignifier takes no arguments. It performs the following steps:
    # 1. Let AR be the Agent Record of the surrounding agent.
    AR = surrounding_agent.agent_record
    # 2. Return AR.[[Signifier]].
    return AR.signifier

# 8.7.2 AgentCanSuspend()
def AgentCanSuspend():
    # The abstract operation AgentCanSuspend takes no arguments. It performs the following steps:
    # 1. Let AR be the Agent Record of the surrounding agent.
    AR = surrounding_agent.agent_record
    # 2. Return AR.[[CanBlock]].
    return AR.can_block
    # NOTE: In some environments it may not be reasonable for a given agent to suspend. For example, in a
    # web browser environment, it may be reasonable to disallow suspending a document's main event
    # handling thread, while still allowing workers' event handling threads to suspend.
