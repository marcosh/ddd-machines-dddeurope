# Domain modelling with state machines

## Who am I

Marco Perone

I work at [Tweag](https://www.tweag.io/)

[http://marcosh.github.io/](http://marcosh.github.io/)

[https://twitter.com/marcoshuttle](https://twitter.com/marcoshuttle)

## Working environment

We will be using [Gitpod](https://gitpod.io/#https://github.com/marcosh/ddd-machines-dddeurope)

## Domain modelling

![the picture that explains everything](http://marcosh.github.io/img/the-picture-that-explains-everything.png)

- <span style="color: orange">Domain events</span>: events relevant for domain experts

- <span style="color: dodgerblue">Commands</span>: user intentions/actions/decisions

- <span style="color: green">Read models</span>: data needed in order to make decisions

- <span style="color: orchid">Policy</span>: reactive logic that takes place after an event

- <span style="color: yellow">Aggregates</span>: responsible to decide what happens on commands

## State machines

When I say `state machine`, I actually mean a `Mealy machine`

```haskell
data Mealy state input output = Mealy
  { initialState :: state
  , action :: state -> input -> (state, output)
  }
```

![Mealy machine](https://upload.wikimedia.org/wikipedia/commons/b/b4/Mealy.png?1653382367553)

### Aggregates

Aggregates are state machines with <span style="color: dodgerblue">commands</span> as inputs and <span style="color: orange">events</span> as outputs

### Projections

Projections are state machines with <span style="color: orange">events</span> as inputs and <span style="color: green">read models</span> as outputs

### Policies

Policies are state machines with <span style="color: orange">events</span> as inputs and <span style="color: dodgerblue">commands</span> as outputs.

Policies are the only part of the system where side effects can take place.

```mermaid
graph LR

command --> aggregate((aggregate))
aggregate --> event
event --> projection((projection))
projection --> readModel
event --> policy((policy))
policy --> command
```

## Our domain

**Risk manager**

```mermaid
stateDiagram-v2
  NoData --> CollectedUserData
  CollectedUserData --> CollectedLoanDetailsFirst
  CollectedUserData --> ReceivedCreditBureauDataFirst
  CollectedLoanDetailsFirst --> CollectedAllData
  ReceivedCreditBureauDataFirst --> CollectedAllData
```

### Aggregate

To define our `Aggregate` we need to define the initial `State` and, for every `State` and `Command`, which is the next `State` and which `Event`s should be emitted.

### Projection

We want to project the data received until the current state of the process, which could or could not be there.

To define our `Projection` we need to define its initial state and how to update its state for every received `Event`.

### Policy

The policy reacts to `Events` and generates new `Command`s, after possibly interacting with the external world.

Our policy retrieves the credit bureau data after the collection of the user data.

It is actually not stateful, so to define it we just need to specify what to do when we receive an `Event` and return a list of `Command`s

## Compositionality

### Feedback

Now that we have our three state machines, we need to connect them.

First e connect the aggregate and the policy

```mermaid
graph LR

command --> aggregate((aggregate))
aggregate --> event
event --> policy((policy))
policy --> command
```

For any <span style="color: dodgerblue">command</span> we execute the <span style="color: yellow">aggregate</span>, and we get some <span style="color: orange">events</span>. We feed those <span style="color: orange">events</span> one by one to the <span style="color: orchid">policy</span> obtaining other <span style="color: dodgerblue">commands</span> which we feed again into the <span style="color: yellow">aggregate</span>, and we continue with the cycle.

Let's call the result `feedback aggregate policy`.

### Category

Now we need to compose the aggregate and the policy with the projection. This is easier, since we just take the <span style="color: orange">events</span> emitted by `feedback aggregate policy`, and we process them with our `projection` to get our <span style="color: green">read model</span>.

### Strong

Given two state machines with the same input type `a` and two different outputs `b` and `c`, we can create a state machine which consumes inputs of type `a` and produces outputs of type `(b, c)`.

## Rendering
