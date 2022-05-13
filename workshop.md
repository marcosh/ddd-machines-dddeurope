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

## State machines

When I say `state machine`, I actually mean a `Mealy machine`

```haskell
data Mealy state input output = Mealy
  { initialState :: state
  , action :: state -> input -> (state, output)
  }
```

## Our domain

Risk manager

```mermaid
stateDiagram-v2
  NoData --> CollectedUserData
  CollectedUserData --> CollectedLoanDetailsFirst
  CollectedUserData --> ReceivedCreditBureauDataFirst
  CollectedLoanDetailsFirst --> CollectedAllData
  ReceivedCreditBureauDataFirst --> CollectedAllData
```

### Aggregate

### Policy

### Projection

## Compositionality

## Rendering