# Introduction and Motivation

This document describes an application-level server/client protocol for playing a Dr. Mario-like multiplayer game over the Internet. In normal usage, it's intended for there to be a single server, which hosts many games, and where each player and spectator has their own, independent client.

There are several occasionally-conflicting design goals for this protocol. Roughly in order from most important to least important, the protocol should be:

* **Robust.** With one player in Canada connected over a satellite connection, one in the Netherlands connected via a crowded, public cafe wireless connection, and a server in the United States, lag and lag spikes are a fact of life. Temporary disconnections also happen from time to time. The game should be fun and playable despite this. Lag issues are addressed by making all controls live client-side, and by giving both the server and clients ways to inform their counterparts of predicted events before they happen, and to indicate whether the predictions are tentative or certain. Conditional predictions (e.g. conditioned on where a pill is locked) are especially important for the client, so that it can begin rendering the outcome of a play without waiting for a complete server-client roundtrip. To make disconnections less devastating, clients can request the full game state at any time, and the server's behavior when there are multiple outstanding connections from a single client is specified.
* **Client-biased.** It is expected that there will be just one implementation of the server part of the protocol, but potentially many implementations of the client part of the protocol. For example, Linux-based, Windows-based, web-based, and AI-backed clients all seem like possibilities. Therefore, to the extent possible, all game logic should be pushed to the server. Clients should not need to reimplement clearing rules, trash gravity, random board generation, etc.
* **Flexible.** Historically, there have been many different Dr. Mario-like games with varying game rules. Implementations have varied the number of players, available moves, win conditions, pill shapes, laws of gravity, and more. The protocol should be generic enough that it can support these and other variant ideas without changes to the protocol. (When somebody figures out what Dr. Mario 99 should be like, hopefully this protocol is suitable!) This is achieved by many of the same mechanisms as being robust and client-based: pushing all game-logic to the server means that variants need only be implemented once in the server implementation, and not replicated by all servers; and giving the server strong prediction capabilities allows clients to know what will happen even if its author has never seen this particular game variant before. Additionally, since controls are both client-side and one of the things varied, the protocol includes a flexible language for the server to communicate control rules to the client.
* **Frugal.** Many players still connect to the Internet with low-bandwidth connections. To support these players, the wire format for the protocol should be compact, fitting the most common messages in as few bytes as possible. Consequently, the protocol defines a high-level data model that includes a concept of "edits" for each kind of data, so that most of the game state can be omitted from most messages, and specifies a low-overhead binary wire format.
* **Complete.** Besides simply playing a game, players will likely expect to be able to perform a number of supporting and related actions. They will want to discover other players who are interested in a game right now (or be informed when one becomes available); negotiate the variant that they will be playing; coordinate the exact timing of the game start so that all players are ready when that happens; play with the same people and game rules multiple times when everybody involved is having fun; abort games early by resigning or demanding a resignation from a non-responsive opponent; watch replays of previously finished games; etc. The protocol will eventually support a wide range of such flows, but see below for caveats about this early version.
* **Focused.** The Internet and its denizens being what they are, perhaps there will eventually be bad actors. Frequent and powerful predictions reduce the problems of lag, but could enable clients to show their users more about what is coming up than was intended. Putting controls client-side helps with keeping the game responsive and predictable, but opens you up to clients that give their operators superhuman maneuvering abilities. (Of course, these problems are not new inventions, created by a move from in-person games to Internet games! There's always been the person that brought a turbo controller or tried to obscure their opponent's view of the screen.) Even with only good actors, problems can arise. For example, somebody may agree to a game, but then have to attend to a puking child instead of playing. Servers may eventually have to implement some mechanisms for enforcing good behavior, for detecting and punishing bad actors, or for accommodating unexpected events happening to good actors, but the protocol does not aim to provide those services on its own. It describes only how clients and servers can understand each other, and not behavioral policies.
* **Fun.** One of the core reasons we play games is to have fun and connect with our fellow humans. Experience with existing services suggests that this urge is so strong that the ability to send even just one or two predefined messages is appreciated and oft-used by players. To this end, the protocol supports some limited in-game communication mechanisms, and is expected to support more extensive out-of-game communication mechanisms in the future.

Historically, software products succeed best when the feature set begins small, and grows only when the existing set has enjoyed some use (and the associated experimentation and bug-fixing). In the interest of helping this project succeed, therefore, this early version of the specification intentionally punts on being complete. It will describe only the part of the protocol involved in playing a single game from beginning to end. The framing that makes that usable -- such as the ability for players and spectators to discover games and negotiate rules -- will be omitted until at least one conforming server and one conforming client exist.