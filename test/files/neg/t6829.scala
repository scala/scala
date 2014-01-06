package bugs

/**
  * Created with IntelliJ IDEA.
  * User: arya
  * Date: 12/18/12
  * Time: 4:17 PM
  * To change this template use File | Settings | File Templates.
  */
object currenttype2 {

   type Reward = Double

   trait AbstractAgent[State,Action] {
     type A = AbstractAgent[State,Action]
     def chooseAction(s: State): Action
     def startEpisode: A = this
     def learn(s1: State, a: Action, s2: State, r: Reward): A
   }

   case class RewardFunction[State,Action](r: (State,Action,State) => Reward)

  trait Rules[G<:GameDomain] {
    def simulate(state: G#State, agentActions: List[(G#Agent,G#Action)]): G#State
  }

  trait AgentSimulation[G<:GameDomain] {
    val agents: List[G#Agent]
    val state: G#State
    val rewards: Map[G#Agent,G#Rewards]
    val rules: Rules[G]
    val pastHistory: List[G#State]
    lazy val currentHistory = state :: pastHistory

    lazy val actions: Map[G#Agent,G#Action] = agents.map(a => a -> a.chooseAction(state)).toMap
    lazy val nextState: G#State = rules.simulate(state, actions.toList)

    def step: AgentSimulation[G]
  }

  case class LearningSimulation[G<:GameDomain](agents: List[G#Agent],
                                               state: G#State,
                                               rewards: Map[G#Agent,G#Rewards],
                                               rules: Rules[G],
                                               pastHistory: List[G#State] = Nil) extends AgentSimulation
  {
    lazy val step: LearningSimulation = {
      val updatedAgents: List[G#Agent] = agents map { agent =>
        val (s,a,s2) = (state,actions(agent),nextState)
        val r = rewards(agent).r(s,a,s2)
        agent.learn(s,a,s2,r): G#Agent
      }
      copy(agents = updatedAgents, state = nextState, pastHistory = currentHistory)
    }
  }

   trait GameDomain {
     domain =>
     type State
     type Action
     type Agent = AbstractAgent[State, Action] // agent supertype
     type Rewards = RewardFunction[State,Action]
   }
 }
