# Introduction and Motivation

This document describes an application-level server/client protocol for playing a Dr. Mario-like multiplayer game over the Internet. In normal usage, it's intended for there to be a single server, which hosts many games, and where each player and spectator has their own, independent client.

There are several occasionally-conflicting design goals for this protocol. Roughly in order from most important to least important, the protocol should be:

* **Robust.** With one player in Canada connected over a satellite connection, one in the Netherlands connected via a crowded, public cafe wireless connection, and a server in the United States, lag and lag spikes are a fact of life. Temporary disconnections also happen from time to time. The game should be fun and playable despite this. Lag issues are addressed by making all controls live client-side, and by giving both the server and clients ways to inform their counterparts of predicted events before they happen. Conditional predictions (e.g. conditioned on where a pill is locked) are especially important for the client, so that it can begin rendering the outcome of a play without waiting for a complete server-client roundtrip. To make disconnections less devastating, clients can request the full game state at any time, and the server's behavior when there are multiple outstanding connections from a single client is specified.
* **Client-biased.** It is expected that there will be just one implementation of the server part of the protocol, but potentially many implementations of the client part of the protocol. For example, Linux-based, Windows-based, web-based, and AI-backed clients all seem like possibilities. Therefore, to the extent possible, all game logic should be pushed to the server. Clients should not need to reimplement clearing rules, trash gravity, random board generation, etc.
* **Flexible.** Historically, there have been many different Dr. Mario-like games with varying game rules. Implementations have varied the number of players, available moves, win conditions, pill shapes, laws of gravity, and more. The protocol should be generic enough that it can support these and other variant ideas without changes to the protocol. (When somebody figures out what Dr. Mario 99 should be like, hopefully this protocol is suitable!) This is achieved by many of the same mechanisms as being robust and client-based: pushing all game-logic to the server means that variants need only be implemented once in the server implementation, and not replicated by all clients; and giving the server strong prediction capabilities allows clients to know what will happen even if its author has never seen this particular game variant before. Additionally, since controls are both client-side and one of the things varied, the protocol includes a flexible language for the server to communicate control rules to the client.
* **Frugal.** Many players still connect to the Internet with low-bandwidth connections. To support these players, the wire format for the protocol should be compact, fitting the most common messages in as few bytes as possible. Consequently, the protocol defines a high-level data model that includes a concept of "edits" for each kind of data, so that most of the game state can be omitted from most messages, and specifies a low-overhead binary wire format.
* **Complete.** Besides simply playing a game, players will likely expect to be able to perform a number of supporting and related actions. They will want to discover other players who are interested in a game right now (or be informed when one becomes available); negotiate the variant that they will be playing; coordinate the exact timing of the game start so that all players are ready when that happens; play with the same people and game rules multiple times when everybody involved is having fun; abort games early by resigning or demanding a resignation from a non-responsive opponent; watch replays of previously finished games; etc. The protocol will eventually support a wide range of such flows, but see below for caveats about this early version.
* **Focused.** The Internet and its denizens being what they are, perhaps there will eventually be bad actors. Frequent and powerful predictions reduce the problems of lag, but could enable clients to show their users more about what is coming up than was intended. Putting controls client-side helps with keeping the game responsive and predictable, but opens you up to clients that give their operators superhuman maneuvering abilities. (Of course, these problems are not new inventions, created by a move from in-person games to Internet games! There's always been the person that brought a turbo controller or tried to obscure their opponent's view of the screen.) Even with only good actors, problems can arise. For example, somebody may agree to a game, but then have to attend to a puking child instead of playing. Servers may eventually have to implement some mechanisms for enforcing good behavior, for detecting and punishing bad actors, or for accommodating unexpected events happening to good actors, but the protocol does not aim to provide those services on its own. It describes only how clients and servers can understand each other, and not behavioral policies.
* **Fun.** One of the core reasons we play games is to have fun and connect with our fellow humans. Experience with existing services suggests that this urge is so strong that the ability to send even just one or two predefined messages is appreciated and oft-used by players. To this end, the protocol supports some limited in-game communication mechanisms, and is expected to support more extensive out-of-game communication mechanisms in the future.

Historically, software products succeed best when the feature set begins small, and grows only when the existing set has enjoyed some use (and the associated experimentation and bug-fixing). In the interest of helping this project succeed, therefore, this early version of the specification intentionally punts on being complete. It will describe only the part of the protocol involved in playing a single game from beginning to end. The framing that makes that usable — such as the ability for players and spectators to discover games and negotiate rules — will be omitted until at least one conforming server and one conforming client exist.

# Typography

When defining a new term, the term's first use will be written with *italics*. Italics are also used for mathematical variables. Text strings are written in `monospace`. If the text contains strange characters, they may be escaped for clarity by surrounding a codepoint, written in decimal, with `\u` and `\`. For example, `my 🐕 is cute` and `my \u128021\ is cute` represent the same text. If a text contains a backslash followed by a "u", one or the other will always be escaped. Byte strings are written in `monospace` as an even-length sequence of hexadecimal digits surrounded by single quotes. For example, `'0cafe101'` represents a four-byte sequence with the bytes 12, 175, 225, and 1. That sequence will never be written as `'cafe101'`. To avoid ambiguity, all strings beginning with a single quote will have that single quote escaped; for example, `\u39\0cafe101'` represents a 9-character text which could be used to represent a byte sequence. Concrete type names are written in **bold**. ALL-CAPS PHRASES such as "MUST" and "MUST NOT" are used as described in RFCs 2119 and 8174.

# Data Model

Every message sent with the damascus protocol has a *type* that describes what values the message can take on. The collection of types available has many influences, including JSON and the theory of algebraic types. Each type is associated with five concepts:

1. A *type name* that will be used to refer to that type in the remainder of this document.
2. An *interpretation*, which is a set-theory description of the values associated with that type.
3. A set of human-readable text strings called *abstract values* that will be used to refer to elements of the interpretation in the remaining sections of this document.
4. A set of compact, machine-readable byte strings called *concrete values*, each of which uniquely identifies an element of the interpretation.
5. A separate *patch* type (and function) for edits that describe a way to transform a value.

Type names are given in parentheses after their subsubsection header. Concrete values are represented in CBOR format as described in RFC 8949. The patch type associated with type *T* can be referred to by Δ*T*. The meaning of patches is given by defining a function *patch* ∈ *T*⨯Δ*T* → *T* for each type.

If you have programmed in any modern language, you can almost certainly learn everything you need to know to understand the rest of this document by reading the table of contents for this section.
On a first read, you might want to skip to the next section, and come back for a more careful review once you need to know the details of patches or encoding into bytes.

In the descriptions of abstract values, the term *whitespace* means a text containing only `\u9\` (tab), `\u10\` (newline), or `\u32\` (space). A *comma-separated sequence* refers to text that contains a sequence of some other specified abstract values with commas in between. Comma-separated sequences are allowed to have arbitrarily many preceding commas, trailing commas, and whitespace around the commas. For example, ` ,, , 35,"35"	` and `,35,"35",` are each a comma-separated sequence of abstract values `35` and `"35"`, but `35,,"35"` is not.

The remaining sections of this document will use abstract values exclusively when talking about inhabitants of a type's interpretation. The remaining subsections of this section will occasionally blur the distinction between a type's name and its interpretation.

## Simple Base Types

### Signed 64-bit Integers (**i64**)

There is just one number type; its interpretation is the set of signed, 64-bit integer values (-2<sup>63</sup> through 2<sup>63</sup>-1 inclusive).

Abstract values are written in decimal or hexadecimal, with an optional sign prefix (assumed positive if missing). For example, `35`, `+35`, `0x23`, and `+0x23` are all abstract number values representing 35, while `-35` and `-0x23` are abstract number values representing -35.

Concrete values use CBOR's number types, major types 0 (positive integers) and 1 (negative integers). Although CBOR can represent larger numbers, messages MUST NOT include values of major type 0 or 1 that fall outside the range -2<sup>63</sup> through 2<sup>63</sup>-1.

A patch is again an **i64**, to be added with the usual two's complement wrap-around rules:

*patch*(*v*, *p*) = *v*+*p* mod 2<sup>64</sup>

(The mod operation chooses its representative from the range -2<sup>63</sup> through 2<sup>63</sup>-1.)

Particular messages may restrict numbers further, such as by demanding that they fit in an unsigned byte (0 through 255 inclusive). This may mean that patches are forced to be out of the bounds for the type they are modifying; for example, with the unsigned-byte restriction, to change from `255` to `0`, one must use the patch `-255`, not `1`, even though `-255` does not fit in a single unsigned byte. As another example, if the number is restricted to the range `100` through `110`, none of the possible patches fall in the same range as the i64 itself.

### Strings (**string**)

The **string** type is interpreted as sequences of Unicode codepoints.

Abstract values are written surrounded by double quotes (with any internal double quotes or backslashes escaped by prefixing a backslash). For example, `"my \u128021\ is cute"` is an abstract string value with 12 codepoints.

Concrete values use CBOR's text string type, major type 3.

An ***atomic string edit*** is a value of a custom type (see the "Custom Types" subsection below for more details on how to interpret this table).

| Type name | Variant | Tag | Fields | Constraints | Meaning
| --------- | ------- | --- | ------ | ----------- | -------
| **atomic string edit**
| | `delete`  | `0`
| |           | | **i64** | non-negative | the index of the first codepoint deleted
| |           | | **i64** | non-negative | the number of codepoints to delete
| | `insert`  | `1`
| |           | | **i64** | non-negative | the index where the first new codepoint will appear
| |           | | **string** | | the text to insert
| | `replace` | `2` | | | a deletion (of the length of the second field) followed by an insertion
| |           | | **i64** | non-negative | the index of the first codepoint to replace
| |           | | **string** | | the new codepoints to write over the old text

All indexing is done by counting codepoints (and not, say, bytes or words used by some specific encoding). The auxiliary function *astrpatch* (short for "atomic string patch") makes the meaning of these precise:

*astrpatch*(*s*, `delete(`*i*`,` *len*`)`) = *s*<sub>0</sub>, ..., *s*<sub>*i*-1</sub>, *s*<sub>*i*+*len*</sub>, ..., *s*<sub>*n*</sub>  
*astrpatch*(*s*, `insert(`*i*`,` *p*`)`) = *s*<sub>0</sub>, ..., *s*<sub>*i*-1</sub>, *p*<sub>0</sub>, ..., *p*<sub>*m*</sub>, *s*<sub>*i*</sub>, ..., *s*<sub>*n*</sub>  
*astrpatch*(*s*, `replace(`*i*`,` *p*`)`) = *s*<sub>0</sub>, ..., *s*<sub>*i*-1</sub>, *p*<sub>0</sub>, ..., *p*<sub>*m*</sub>, *s*<sub>*i*+*m*+1</sub>, ..., *s*<sub>*n*</sub>

In the above equations, the variables *m* and *n* stand in for the lengths of *p* and *s*, respectively. The usual math convention — where out-of-bound indexes are simply dropped from the sequence — applies here. A **string** patch is a sequence of **atomic string edit**s, to be applied in order.

*patch*(*s*, `[`*p*<sub>0</sub>`,` ...`,` *p*<sub>*n*</sub>`]`) = *astrpatch*(*astrpatch*(*astrpatch*(*s*, *p*<sub>0</sub>), ...), *p*<sub>*n*</sub>)

Note that some messages may place additional restrictions on strings it contains; for example, by specifying a minimum or maximum length. Although the associated patch message is REQUIRED to have a patch that results in a string that satisfies those restrictions, individual atomic string edits may temporarily cause the construction of a sequence that does not. For example, if a string is required to have three codepoints, a patch that deletes a codepoint and inserts a new codepoint is allowed, even though there is an intermediate sequence of just two codepoints.

## Container Base Types

### Sequences ([*T*])

The type [*T*] is interpreted as sequences of values, where each value is from the interpretation of *T*.

Abstract values are a comma-separate sequence of abstract values for *T* surrounded by square brackets. For example, `[35,-35]`, `[ 35, -35 , ]`, `[ ,35,-35]`, and `[, 35,    \u9\\u10\-35,]` are all abstract values representing the same thing.

Concrete values use CBOR's sequence type, major type 4. Unlike CBOR, in damascus sequence elements all have the same type (but see "Custom Types" below, which also use major type 4 and do not have this restriction).

An *atomic sequence edit* is a value of a custom type (see the "Custom Types" subsection below for more details on how to interpret this table).

| Type name | Variant | Tag | Fields | Constraints | Meaning
| --------- | ------- | --- | ------ | ----------- | -------
| atomic sequence edit for *T*
| | `delete` | `0`
| |          | | **i64** | non-negative | the index of the first element deleted
| |          | | **i64** | non-negative | the number of elements to delete
| | `insert` | `1`
| |          | | **i64** | non-negative | the index where the first new element will appear
| |          | | [*T*] | | the elements to insert
| | `modify` | `2` | | | in-place mutation
| |          | | **i64** | non-negative | the index of the first element to modify
| |          | | [Δ*T*] | | the patches to apply to elements

The auxiliary function *aseqpatch* (short for "atomic sequence patch") makes the meaning of these precise:

*aseqpatch*(`[`*s*<sub>0</sub>`,` ...`,` *s*<sub>*n*</sub>`]`, `delete(`*i*`,` *len*`)`) = `[`*s*<sub>0</sub>`,` ...`,` *s*<sub>*i*-1</sub>`,` *s*<sub>*i*+*len*</sub>`,` ...`,` *s*<sub>*n*</sub>`]`  
*aseqpatch*(`[`*s*<sub>0</sub>`,` ...`,` *s*<sub>*n*</sub>`]`, `insert(`*i*`, [`*p*<sub>0</sub>`,` ...`,` *p*<sub>*m*</sub>`])`) = `[`*s*<sub>0</sub>`,` ...`,` *s*<sub>*i*-1</sub>`,` *p*<sub>0</sub>`,` ...`,` *p*<sub>*m*</sub>`,` *s*<sub>*i*</sub>`,` ...`,` *s*<sub>*n*</sub>`]`  
*aseqpatch*(`[`*s*<sub>0</sub>`,` ...`,` *s*<sub>*n*</sub>`]`, `modify(`*i*`, [`*p*<sub>0</sub>`,` ...`,` *p*<sub>*m*</sub>`])`) = `[`*s*<sub>0</sub>`,` ...`,` *s*<sub>*i*-1</sub>`,` *patch*(*s*<sub>*i*+0</sub>, *p*<sub>0</sub>)`,` ...`,` *patch*(*s*<sub>*i*+*m*</sub>, *p*<sub>*m*</sub>)`,` *s*<sub>*i*+*m*+1</sub>`,` ...`,` *s*<sub>*n*</sub>`]`

The usual math convention — where out-of-bound indices are simply dropped from the sequence — applies here. A [*T*] patch is a sequence of atomic sequence edits, to be applied in order.

*patch*(*s*, `[`*p*<sub>0</sub>`,` ...`,` *p*<sub>*n*</sub>`]`) = *aseqpatch*(*aseqpatch*(*aseqpatch*(*s*, *p*<sub>0</sub>), ...), *p*<sub>*n*</sub>)

Note that some messages may place additional restrictions on sequences it contains; for example, by specifying a minimum or maximum length. Although the associated patch message is REQUIRED to have a patch that results in a sequence that satisfies those restrictions, individual atomic sequence edits may temporarily cause the construction of a sequence that does not. For example, if a sequence is required to have three elements, a patch that deletes an element and inserts a new element is allowed, even though there is an intermediate sequence of just two elements.

### Dictionaries ({*K*:*V*})

The type {*K*:*V*} is interpreted as the collection of finitely supported partial functions *K* → *V* (commonly known as a *dictionary* with key type *K* and value type *V*).

A *mapping* for *k*⟼*v* is given by putting a colon (and, optionally, whitespace around the colon) between the abstract values for *k* and *v*. An abstract value for a function *f* in type {*K*:*V*} has curly brackets around a comma-separated sequence that has exactly one mapping for *k*⟼*f*(*k*) for each *k* in *f*'s support. For example, `{, 35 : "35", 50:"50",  40:"40"}` is an abstract value representing a function that tells how to convert a few particularly round numbers to their string representations. No particular key order is required.

Concrete values use CBOR's map type, major type 5. Unlike CBOR, in damascus, all keys of a dictionary must have the same type, as do all values.

An *atomic dictionary edit* is a custom type that describes how to update the mapping for a particular key.

| Type name | Variant | Tag | Fields | Constraints | Meaning
| --------- | ------- | --- | ------ | ----------- | -------
| atomic dictionary edit for {*K*:*V*}
| | `delete` | `0` | | | remove a key (and its associated value)
| | `insert` | `1` | | | insert (or overwrite) the given value at this key
| |          | | *V*
| | `modify` | `2` | | | update the value at this key in-place (equivalent to a `copy` from the same key)
| |          | | Δ*V*
| | `copy`   | `3` | | | insert (or overwrite) with the value from another key
| |          | | *K* | | this key has a starting value similar to the new value we want to create
| |          | | Δ*V* | | when necessary, this allows small updates to the value after it is copied

The type of patches for dictionaries then associates keys with edits, that is, a patch for {*K*:*V*} has type {*K*:atomic dictionary edit for {*K*:*V*}}. The updates are all performed with access to the old value; for example, this means that if a patch updates key `0` and copies key `0`, the copy will always start from the old value, never the new one. In detail:

| *p*(*k*) | *d*(*k*) | *d*(*k'*) | *patch*(*d*,*p*)(*k*) |
| -------- | -------- | --------- | --------------------- |
| `delete()` | anything | | undefined |
| `insert(`*v*`)` | anything | | *v* |
| `modify(`*Δv*`)` | undefined | | undefined |
| `modify(`*Δv*`)` | *v* | | *patch*(*v*,*Δv*) |
| `copy(`*k'*`,`*Δv*`)` | anything | undefined | undefined |
| `copy(`*k'*`,`*Δv*`)` | anything | *v* | *patch*(*v*,*Δv*) |
| undefined | undefined | | undefined |
| undefined | *v* | | *v* |

(The intention is that exactly one row of the above table applies to each key in *K*. If this is not the case, it is a bug in the specification and the maintainer would like to hear about it.)

## Custom Types (various, textual names)

For the algebraically minded, custom types are sums of products with an implicit top-level least fixpoint. Rust programmers will recognize these as enumerations; Haskell programmers as strict data types. Each custom type has a collection of *variants*, and each variant has a sequence of types that serve as its *fields*. As custom types are introduced in this document, they will be presented with a table that looks like this example:

| Type name | Variant | Tag | Fields | Constraints | Meaning
| --------- | ------- | --- | ------ | ----------- | -------
| **server step**
| | `pill` | `0` | | | The player is being given control of a new pill.
| |        | | **i64** | non-negative and smaller than the board width | the x position of the bottom left of a new pill
| |        | | **i64** | non-negative and smaller than the board height | the y position of the bottom left of a new pill
| |        | | **i64** | non-negative and smaller than the number of available pill shapes | the shape of a new pill, as an index into the list of available pill shapes
| |        | | **[i64]** | has an appropriate length for the given pill shape | the colors of each part of the pill
| | `new board` | `1` | | | The player must wait while the board changes.
| |             | | **[[cell]]** | is of size board width ⨯ board height | the cells for a new state of the board

(All custom types in this subsection are for expository purposes only, scoped only to this subsection, and not guaranteed to be consistent with similarly-named types from elsewhere in this document.)

To avoid ambiguity in abstract values, the variant names will not begin or end with whitespace. To avoid ambiguity in concrete values, the variants' tags will be pairwise unequal. All tags are of type **i64**.

For each variant, we can construct a set of tuples. These tuples have one more element than there are fields, and consist of the variant name followed by an interpretation of each field type. The interpretation of the new custom type is the union of these sets over all variants. Custom types are permitted to recursively mention themselves (or other mutually recursive custom types) as fields; the interpretation uses a least fixed-point construction.

TODO: after nailing down the format of cells, come back and make this `new board` message more interesting (and its followup in the concrete value discussion)

An abstract value is constructed by including the variant name, optional whitespace, and a comma-separated sequence of the abstract values for the fields surrounded by parentheses. For example, `pill(3, 7, 0, [1, 2])` describes a new pill of the default shape coming under control at position (3, 7) with blue and red halves, and `new board([[0, 0], [0, 0]])` describes the player being shown a blank 2⨯2 board.

In the simplest case, a concrete value uses CBOR's array type, major type 4. The array contains a concrete value for a tag followed by concrete values for each of the associated variant's fields. For example, the abstract value `pill(3, 7, 0, [1, 2])` could be encoded to the 8-byte concrete value `'8500030700820102'`, and the abstract value `new board([[0, 0], [0, 0]])` could be encoded to the 9-byte concrete value `'820182820000820000'`.

Two exceptions are made to this pattern. First, if there is exactly one variant, the tag MUST be omitted from the array, yielding an array one element shorter than usual. Second, if the result is an array of exactly one element, the array wrapping mechanism MUST be omitted, causing the concrete value to be that element directly. For example, consider these hypothetical types:

| Type name | Variant | Tag | Fields | Constraints | Meaning
| --------- | ------- | --- | ------ | ----------- | -------
| **possible i64**
| | `nothing` | `0` | | | no number available
| | `just`    | `1` | | | there is a number available
| |           | | **i64**
| **color**
| | `black`  | `0`
| | `blue`   | `1`
| | `red`    | `2`
| | `yellow` | `3`
| **shape**
| | `shape`  | `0`
| |          | | **i64** | fits in an unsigned byte | an index into the sequence of available shapes

The abstract value `just(3)` would use the normal pattern, and so might be encoded to the concrete value `'820103'`, say. But since `nothing()` doesn't have any fields, it would be a 1-element array with just the tag, and so is encoded not as an array but as the tag directly, meaning the concrete value `'00'` would be one possible corresponding concrete value.

Like `nothing()`, the abstract values `black()` and friends have no fields, so the concrete values `'00'`, `'01'`, `'02'`, and `'03'` would be correct encodings of the variants of a color.

Since there is just one variant in the shape type, the abstract value `shape(3)` would not include the tag in its concrete value. Since there is just one field, this means there would be just one element if we were to use an array, and so we simply include the field directly. This means that `'03'` is an example of a correct concrete value for `shape(3)`.

Contrast the concrete values for `shape(3)` and `just(3)`, which both have just one field; however, with `shape(3)` the tag is omitted because there are no other variants, while with `just(3)` there are other variants and so the tag is not omitted. Contrast also the concrete values for `shape(3)` and `yellow()`, which are both bare values and not CBOR arrays; however, because the color type has many variants, the bare value must be a tag, and because the shape type has a single variant, the bare value must be a field.

The **shape** type described above is a case of particular interest. The rules described above mean that although **shape** is notionally distinct from **i64**, it nevertheless has the same representations in concrete values; that is, there is no overhead associated with introducing extra levels of aliasing in this way.

Patches allow for patching fields within a variant, providing a complete new value (potentially a different variant), or doing nothing. In detail: suppose the type under consideration is *T*. The type Δ*T* is also a custom type. It has one variant for each variant of *T* with at least one field. The new variant name has `Δ` prepended, and the new tag is one bigger. The new variant has as many fields as the old one; their types are the patch types for the old variant's fields. If *T* does not have exactly one variant, then Δ*T* also includes two new variants: `no change`, with no fields and a tag one smaller than the smallest of the other variants of Δ*T* (or `0` if all variants of *T* have zero fields); and `replace`, with a field of type *T* and a tag one larger than the largest of the other variants of Δ*T* (or `1` if all variants of *T* have zero fields). The following table gives examples for each of the custom types previously discussed:

| Type name | Variant | Tag | Fields
| --------- | ------- | --- | ------
| **Δserver step**
| | `no change` | `0`
| | `Δpill` | `1`
| |         | | **Δi64** (=**i64**)
| |         | | **Δi64**
| |         | | **Δi64**
| |         | | **Δ[i64]**
| | `Δnew board` | `2`
| |              | | **Δ[[cell]]**
| | `replace` | `3`
| |           | | **server step**
| **Δpossible i64**
| | `no change` | `1`
| | `Δjust`     | `2`
| |             | | **Δi64**
| | `replace`   | `3`
| |             | | **possible i64**
| **Δcolor**
| | `no change` | `0`
| | `replace`   | `1`
| |             | | **color**
| **Δshape**
| | `Δshape`  | `1`
| |           | | **Δi64**

As with concrete values, the patches for a custom type that simply wraps another type have no associated overhead; their concrete values are exactly the same as the concrete values for patches to the wrapped type.

This protocol will not use patches for a type if that would cause the `no change` or `replace` tags to be out of the **i64** range.

The *patch* implementation does the obvious thing. When given a collection of field patches for the wrong variant, the patch is ignored.

*patch*(*v*, `replace(`*v'*`)`) = *v'*  
*patch*(*V*`(`*x*<sub>0</sub>`,` ...`,` *x*<sub>*n*</sub>`)`, `Δ`*V*`(`*p*<sub>0</sub>`,` ...`,` *p*<sub>*n*</sub>`)`) = *V*`(`*patch*(*x*<sub>0</sub>`,` *p*<sub>0</sub>)`,` ...`,` *patch*(*x*<sub>*n*</sub>, *p*<sub>*n*</sub>)`)`  
*patch*(*v*, *p*) = *v*        in all other cases (including when *p* = `no change()`)

# Rambling

stuff that happens in-game: message id's 0-23
stuff where latency/bandwidth doesn't matter: bigger id's

if you like, can think of client messages as one big custom type, and server messages as another big custom type

## Version negotiation

server message id 24, propose version, field **[string]**  
client message id 24, accept version, field **string**  
client message id 25, reject all versions, no fields

## Full state description

60 fps for now, part of the negotiable setup options in future specs

server message id 25, current state, fields **i64** for the latest frame the server has committed to and **{string: player state with predictions}** for the individual states of each participating player (see below)

### Core types

A ***cell*** describes what to draw at one grid location on the board, and includes a shape and a color.

| Type name | Variant | Tag | Fields | Constraints | Meaning
| --------- | ------- | --- | ------ | ----------- | -------
| **cell**
| | `cell` | `0`
| | | | **i64** | in the range [0,17] | shape information; see below
| | | | **i64** | | color information

There are three classes of shapes: pill components (0-15), viruses (16), and unoccupied spaces (17). The pill components are expressed as a four-bit field describing whether the component should be drawn as connecting to its neighbors in each of the four directions, with a 1 bit indicating a connection and a 0 bit indicating no connection. With bit 0 being the least significant bit:

| Bit | Direction
| --- | ---------
| 0 | positive x
| 1 | negative x
| 2 | positive y
| 3 | negative y

For the color information, clients SHOULD interpret the following values specially:

| Value | Color
| ----- | -----
| 0 | fully transparent, i.e. do not draw anything if rendering this to an image
| 1 | blue
| 2 | red
| 3 | yellow
| 4 | cyan
| 5 | magenta

Servers SHOULD restrict the colors in their messages to the range [0,5] except as noted below. When using the unoccupied shape, servers SHOULD use the color 0. When using the color 0, servers SHOULD use the unoccupied shape.

It will often be convenient to have a notion of ***point*** and ***vector***. These will always be 2D versions of their math counterparts, and will have the same representation as each other as abstract values; the distinction between them is semantic, with a **point** representing an absolute position in the 2D space and a **vector** representing an offset or motion.

| Type name | Variant | Tag | Fields | Constraints | Meaning
| --------- | ------- | --- | ------ | ----------- | -------
| **point**
| | `point` | `0`
| | | | **i64** | | *x* coordinate
| | | | **i64** | | *y* coordinate
| **vector**
| | `vector` | `0`
| | | | **i64** | | *x* offset
| | | | **i64** | | *y* offset

When talking about pills, sometimes it is important to talk only about the *content* of the pill — the cells it has, given in relation to a local origin — and sometimes it is more relevant to talk about a located version that also says where on the board the pill should be rendered. With that in mind, a ***pill content*** describes a collection of cells that the client can manipulate (e.g. by moving or rotating it), while a ***pill*** is the located version.

For uniformity, many types will demand that a nested list will have a certain class of element *clipped*. This means that there is at least one element outside of that class in the first row, and at least one element outside of that class in the first column. That is, the list must be nonempty, the first element of the list must contain an element not in the class, and there must be an element (which is itself a list) with a first element that is not in the class.

| Type name | Variant | Tag | Fields | Constraints | Meaning
| --------- | ------- | --- | ------ | ----------- | -------
| **pill content**
| | `content` | `0`
| | | | **[[cell]]** | unoccupied cells clipped
| **pill**
| | `pill` | `0`
| | | | **point** | | the location of the pill's local origin on a board
| | | | **pill content**

The outer list of lists should be interpreted as varying along the x axis, with earlier elements being at lower coordinates; similarly, each inner list of cells should be interpreted as varying along the y axis, with earlier elements being at lower coordinates.

A ***board*** describes the backdrop on which an individual player's game is taking place.

| Type name | Variant | Tag | Fields | Constraints | Meaning
| --------- | ------- | --- | ------ | ----------- | -------
| **board**
| | `board` | `0`
| | | | **i64** | positive | board width
| | | | **i64** | positive | board height
| | | | **[[cell]]** | outer list must have length equal to first field; all inner lists must have the length equal to second field | board content

The terms *in-bounds* and *out-of-bounds* are used in the obvious way to relate **point**s and **board**s. The **point** `(`*x*`,` *y*`)` is in-bounds for `board(`*w*`,` *h*`,` *c*`)` if 0≤*x*<*w* and 0≤*y*<*h* and out-of-bounds otherwise.

Similarly to **pill content**, the content of the board should be interpreted with the outer list varying along the x axis, starting at coordinate 0 and increasing, while each inner list should be interpreted as varying along the y axis, starting at coordinate 0 and increasing.

Individual player's clients are always in one of two states: either there is a pill currently under the player's control, or they are watching an animation and waiting for the next pill to be controllable. Although clients are locally responsible for moving the pill, they are required to prove that a pill can actually be placed where they claim it currently is. This is manifested by giving the history of moves since the pill came under their control.

| Type name | Variant | Tag | Fields | Constraints | Meaning
| --------- | ------- | --- | ------ | ----------- | -------
| **control**
| | `wait` | `0`
| | `control` | `1`
| | | | **i64** | non-negative | the first frame on which the player had control of their latest pill
| | | | **pill** | | the starting pill placed under player control
| | | | **{i64: string}** | keys must be non-negative | the moves made by the player; keys are frame offsets from the first field

There is rudimentary support for in-game communication. It seems unreasonable to expect players to type during the game; additionally, there are some common messages that clients may want to interpret specially, such as by playing a sound effect. To accommodate these two concerns, besides free-form text, there are special message forms for a few selected emotions.

| Type name | Variant | Tag | Fields | Constraints | Meaning
| --------- | ------- | --- | ------ | ----------- | -------
| **message content**
| | `text` | `0`
| | | | **string**
| | `impressed` | `1`
| | `proud` | `2`
| | `bored` | `3`
| **message**
| | `message` | `0`
| | | | **i64** | | frame the message was sent
| | | | **string** | | sending player
| | | | **message content**

Like most languages, this one has booleans.

| Type name | Variant | Tag | Fields | Constraints | Meaning
| --------- | ------- | --- | ------ | ----------- | -------
| **bool**
| | `false` | `0`
| | `true` | `1`

### Movement rules

#### Types

 A ***pill shape*** is just like a **pill content**, except its color information is interpreted differently (more on this in a moment).

| Type name | Variant | Tag | Fields | Constraints
| --------- | ------- | --- | ------ | -----------
| **pill shape**
| | `shape` | `0`
| | | | **[[cell]]** | unoccupied cells clipped; the recommendation that servers restrict colors to [0,5] does not apply to these cells

A ***pill motion*** describes how to change a pill that's under control; it includes components for motion and rotation. *Forcing* is when a particular movement is required every so often (also called "gravity" when the force direction is down).

| Type name | Variant | Tag | Fields | Constraints | Meaning
| --------- | ------- | --- | ------ | ----------- | -------
| **pill motion**
| | `variant` | `0`
| | | | **[vector]** | | spaces on the board that must be in-bounds and unoccupied for this variant to apply
| | | | **vector** | | how to move the origin of the pill
| | | | **pill shape** | | how to perform a rotation
| **movement rule**
| | `movement` | `0`
| | | | **[string]** | | extra forcing counters to reset
| | | | **bool** | | whether this motion can automatically be repeated many times in a single frame
| | | | **bool** | | whether attempting and failing to make this move ends control of the pill
| | | | **{pill shape: [pill motion]}** | keys must not mention the same color twice; the **pill shape**s in the values must only mention colors used in the key they are associated with; see the reference section for an additional uniqueness constraint
| **force**
| | `force` | `0`
| | | | **string** | | the motion that's forced
| | | | **i64** | | how many frames may pass before the player is forced to make the motion
| **movement rules**
| | `movements` | `0`
| | | | **[force]**
| | | | **{string: movement rule}**

Each **movement rule** has a name, which MAY be an arbitrary string. However, to help clients set up sensible bindings before the game rules have been communicated to it, there are a few conventions about how these names SHOULD be chosen by the server. Pure forms of the motions should be named with either a `"+"` or a `"-"`, followed by an axis chosen from `"x"` for horizontal movement, `"y"` for vertical movement, or `"θ"` for rotation. Rotations use the math convention: positive rotation is counterclockwise, negative is clockwise. If the ruleset allows clients to perform multiple pure motions in a single frame, the pure names should be listed in the order `"x"`, then `"y"`, then `"θ"`. For example, a down-right move would use the name `"+x-y"`, and a left-clockwise move would be named `"-x-θ"`.

Before explaining in detail the meaning of each field, it is instructive to see a simple example, which describes a set of moves similar to the ones available in NES Dr. Mario.

#### Worked example

Consider moving a horizontal pill to the left. The client needs to check that the two spaces where the pill will end up are unoccupied; those two spaces are the current origin and one space to the left of it. The vectors that represent that are `[vector(0,0), vector(-1,0)]`. If they're open, the pill should move one space left, by `vector(-1,0)`. The content will remain unchanged. In a moment, this motion is going to be placed in a dictionary with `shape([[cell(1,1)], [cell(2,2)]])` as the key. The meaning of that shape will be explained then, but for the meantime, to keep the content unchanged we'll copy that shape into the third field. This means we have the following **pill motion**:

    variant(
        , [vector(0, 0), vector(-1, 0)]
        , vector(-1, 0)
        , shape([[cell(1, 1)], [cell(2, 2)]])
        )

For this moveset, we'll be maintaining the invariant that the locations of the cells in the current pill will always be unoccupied; so the server could choose to elide the `vector(0, 0)` occupancy-check, as in:

    variant(
        , [vector(-1, 0)]
        , vector(-1, 0)
        , shape([[cell(1, 1)], [cell(2, 2)]])
        )

There are no other alternative ways to interpret a left movement of a horizontal pill, so this is the only variant that will be listed in the **pill motion**.

This kind of movement should only be considered when the pill is horizontal to begin with. The server indicates this by putting the variant above in a dictionary, associated with a key describing horizontal pills. A horizontal pill will have shape information of `1` (meaning "connect in the positive x direction (to the right)") at the pill origin, and shape information of `2` (meaning "connect in the negative x direction (to the left)") just to the right of the pill origin. What color information will the pill have? While it would be possible to have the protocol demand separate movement rules for each possible coloring of a pill shape, this would lead to a great deal of redundancy. So instead, the pill shape will use some arbitrary colors, all different. If the shape in the movement rule matches the shape of the current pill, the client will remember which movement rule color corresponds to which actual color. For example, consider the following **pill shape**:

    shape([[cell(1,10)],[cell(2,20)]])

If the actual **pill content** has yellow in its left half and red in its right half, it is:

    content([[cell(1,3)],[cell(2,2)]])

When matching that pill shape with this pill, the client will remember that shape color `10` corresponds with pill color `3` (resp. `20` with `2`), and use that mapping when interpreting the **cell**s in the **pill motion** it chooses to produce the new pill.

Describing how to move a vertical pill to the left follows a very similar structure. The main difference here is that we check different positions for occupancy, and the shape information indicates vertical connections rather than horizontal ones.

    variant(
        , [vector(-1, 0), vector(-1, 1)]
        , vector(-1, 0)
        , shape([[cell(4, 1), cell(8, 2)]])
        )

There are three fields left to complete the description of the rule for moving a pill to the left. Forcing will be discussed further below; for now, a simple empty list will do for the first field. The server can put `false()` for the automatic repeat field to indicate that clients should only move pills to the left once in each frame; also, bumping up against a virus or the edge of the board is fine, so it will also put `false()` in the deadly field. This leads to a completed **movement rule**:

    movement([], false(), false(), {
        , shape([[cell(1, 1)], [cell(2, 2)]]): [variant(
            , [vector(-1, 0)]
            , vector(-1, 0)
            , shape([[cell(1, 1)], [cell(2, 2)]])
            )]
        , shape([[cell(4, 1), cell(8, 2)]]): [variant(
            , [vector(-1, 0), vector(-1, 1)]
            , vector(-1, 0)
            , shape([[cell(4, 1), cell(8, 2)]])
            )]
        })

Each motion requires a name. Moving left would conventionally be given the name `"-x"`. If moving left were the only action available to clients, then the server might send the following **movement rules**:

    movements([], {
        , "-x": movement([], false(), false(), {
            , shape([[cell(1, 1)], [cell(2, 2)]]): [variant(
                , [vector(-1, 0), vector(-1, 1)]
                , vector(-1, 0)
                , shape([[cell(4, 1), cell(8, 2)]])
                )]
            , shape([[cell(4, 1), cell(8, 2)]]): [variant(
                , [vector(0, 0), vector(-1, 0)]
                , vector(-1, 0)
                , shape([[cell(1, 1)], [cell(2, 2)]])
                )]
            })
        })

Rotations are handled by having the pill shape in a rule's dictionary not match its key. For example, the clockwise rotation of a horizontal pill might be described with a dictionary that looks like this:

    {
        shape([[cell(1, 1)], [cell(2, 2)]]): [variant(
            , [vector(0, 1)]
            , vector(0, 0)
            , shape([[cell(4, 2), cell(8, 1)]])
            )]
    }

Even though the key has shape information describing a horizontal pill, the result shape information describes a vertical pill. The color information is also worth some attention. The result shape uses color `2` — that is, whatever color was in the right half of the original horizontal pill content — for its lower half, and `1` — the color of the original left half — for its upper half.

Kicks are handled by having multiple variants listed. For example, rotating clockwise from vertical to horizontal might look like this:

    {
        shape([[cell(4, 1), cell(8, 2)]]): [
            , variant(
                , [vector(1, 0)]
                , vector(0, 0)
                , shape([[cell(1, 1)], [cell(2, 2)]])
                )
            , variant(
                , [vector(-1, 0)]
                , vector(-1, 0)
                , shape([[cell(1, 1)], [cell(2, 2)]])
                )
            ]
    }

When there are multiple variants, they are attempted in order until one succeeds. (If none succeed, the motion fails, and the pill remains unchanged.)

A game that only had the motions described so far would be boring, not only because you could only move left and rotate clockwise, but also because the pill would never lock in place! To accomodate that, the server SHOULD include at least one movement rule with its deadly field set to `true()`. In NES Dr. Mario, this is the down movement. This is conventionally called `"-y"`, so the server might send this as part of its movement rules dictionary:

    { "-y": movement([], false(), true(), {
        , shape([[cell(1, 1)], [cell(2, 2)]]): [variant(
            , [vector(0, -1), vector(1, -1)]
            , vector(0, -1)
            , shape([[cell(1, 1)], [cell(2, 2)]])
            )]
        , shape([[cell(4, 1), cell(8, 2)]]): [variant(
            , [vector(0, -1)]
            , vector(0, -1)
            , shape([[cell(4, 1), cell(8, 2)]])
            )]
        })
    }

If the server wanted to allow hard drop for this game, it could include this as part of its dictionary instead; the only difference is that the second field of the **movement rule** is `true()`:

    { "-y": movement([], true(), true(), {
        , shape([[cell(1, 1)], [cell(2, 2)]]): [variant(
            , [vector(0, -1), vector(1, -1)]
            , vector(0, -1)
            , shape([[cell(1, 1)], [cell(2, 2)]])
            )]
        , shape([[cell(4, 1), cell(8, 2)]]): [variant(
            , [vector(0, -1)]
            , vector(0, -1)
            , shape([[cell(4, 1), cell(8, 2)]])
            )]
        })
    }

Some moves can be forced on the player even if they do not want to take them. For example, gravity is a forced downward motion. This rule can be specified by including a **force** in the appropriate list. A **force** associates a movement with a wavelength *λ*; if the player hasn't voluntarily made that move in the last *λ* frames, it is forced on them. For example, the NES begins its HI speed by moving the pill down at least every 14 frames, which could be indicated by the server like this:

    movements([force("-y", 14)], {})

(For brevity, the full dictionary describing all the movement rules has been elided.)

The NES famously allows the player to move left twice by doing a left move and a rotation at the same time. Such a rule could be described by including rules for `"-x+θ"` and `"-x-θ"` with modified occupancy checks and origin offsets for vertical pills. For the purposes of forcing, some compound moves, like a down-clockwise move, may be intended to qualify as another kind of move (in this case down), resetting the frame counter for that kind of forcing. This can be indicated using the first field of the associated **movement rule**, as in:

    { "-y-θ": movement(["-y"], false(), true(), {}) }

Each movement always resets its own forcing counter, so the server MAY NOT include it in the list, as is done frequently above.

The attentive reader may have noticed that no mention has been made thus far of DAS rules. As control is client-side, and this is a mechanism for selecting which controls the player wants to send, all decisions about how to handle DAS are left to individual clients.

#### Movement reference

While a pill is being maneuvered, the state that the client must keep track of has these components:

* a **pill content** *c* that is currently being maneuvered by the player
* a **point** *p* that gives the location of the pill origin (which is always the first element of the first element) on the board
* a **[force]** *f*, distinct from the **[force]** available in the **movement rules**, describing the current number of frames the player has before various kinds of moves are forced on them

Besides the state, which is updated while maneuvering the pill, there is also a **board** *b* and **movement rules** *m* that remain the same throughout. The main information needed from *b* is which points are unoccupied and which points are out-of-bounds, while the movement rules describe what kinds of maneuvers are available on each frame.

The state is initialized by the server, which reports *c* and *p* to the client, with the initial value for *f* being a copy of the **[force]** available in *m*.

The state is updated on each frame. Each frame update is composed of exactly one *forcing update*, up to one optional *motion update* selected by the player, and an arbitrarily long sequence of motion updates caused by the forcing update. The updates occur in that order.

##### Forcing update

When a **force** in *f* is *reset*, it is replaced (at the same position) by a fresh copy of the **force** at the same index in *m*. For example, if *m* contains `[force("-y", 14), force("+x", 22), force("+x", 23)]` and *f* contains `[force("-y", 7), force("+x", -1), force("+x", 0)]`, then resetting the second element results in *f* containing `[force("-y", 7), force("+x", 22), force("+x", 0)]`.

In the forcing update, each **i64** in *f* is decremented by one, then reset if needed by the player's selected movement (see below). If the **i64** is under `0` after both of those changes, two followup actions happen.

* A motion update for the **string** it's associated with (and no repetition, i.e. `false()`) is scheduled for after the player's selected motion update (if any). When multiple **force**s drop under `0` simultaneously, their motion updates occur in the same order as they appear in *f*.
* The **force** is reset.

When the player selects a movement, every **force** with an identical **string** is reset. Additionally, if that movement is associated with a **movement rule** in *m*, then every **force** with a **string** listed in the movement rule's **[string]** field is reset.

##### Motion update

Each motion update is identified by a movement, that is, **string** key into the dictionary in *m*, and a **bool** indicating whether to repeat the motion. A motion update may *succeed* or *fail*, though even failed motion updates may change the state. If the movement **string** is not in *m*'s dictionary, the motion update is considered to succeed without changing the state at all. (A rule for failing without changing the state can be created by giving an empty dictionary in a **movement rule**.)

Once a **movement rule** has been identified, a *matching* process to choose a variant based on the current pill and surrounding spaces on the board begins. If a match is found, steps are taken, possibly in a loop.

###### Matching

The *color-erased* version of a **cell** `cell(`*shape*`, `*color*`)` is simply the contained shape information *shape*. The color-erased versions of **pill content**s and **pill shape**s are constructed by erasing the colors of all the contained **cell**s; the result in both cases has type **[[i64]]**. A **pill content** matches a **pill shape** if their color-erased versions are equal. The server MUST guarantee that no two **pill shape** keys in a **movement rule** are equal after erasing color.

A **vector** *v* matches point *q* if *q*+*v* is in-bounds for *b* and *b* has an unoccupied shape at *q*+*v*. Recall that points both below *and* above the board are considered out-of-bounds. To emulate existing implementations, where pills may be placed halfway out-of-bounds one row above the top of the board, a server might report a slightly larger board with one extra row, start pills one row below the top, and never produce occupied spaces on the top row. A **pill motion** matches point *q* if all the **vector**s in its first field match *q*.

The matching process then proceeds as follows:

1. The keys of the dictionary in the **movement rule** are scanned for a shape that matches *c*.
2. The first **pill motion** in the associated list that matches *p* is selected.

###### Stepping

If either there is no matching key or no matching motion, the current step fails without changing the state. Otherwise, there is a match `variant(`*vs*`, `*v*`, `*s'*`)` under key *s*. The position is updated by adding *v*, that is, *p* := *p*+*v*. The pill content is updated by re-coloring *s'*:

*c*<sub>*ij*</sub> := `cell(`*shape*(*s'*<sub>*ij*</sub>)`,`*color*(*c*<sub>*i'j'*</sub>)`)` iff *color*(*s*<sub>*i'j'*</sub>) = *color*(*s'*<sub>*ij*</sub>)

(The *shape* and *color* functions here extract the appropriate field from their argument.) In other words, to recolor a cell from *s'*, look for the position in *s* with the same color, then use the color from that position in the original pill content *c*. An example is in order. Suppose we have:

*c* = `content([[cell(1,3)],[cell(2,2)]])`  
*s* = `shape([[cell(1,5)],[cell(2,6)]])`  
*s'* = `shape([[cell(4,6),cell(8,5)]])`

So *c* has horizontal pill content (shapes `1` and `2`) with yellow on its left half and red on its right half (colors `3` and `2`); *s* is the shape of a horizontal pill that associates `5` with the color of the left half and color `6` with the right half; and *s'* is the shape of a vertical pill (shapes `4` and `8`) that uses whatever is associated with `6` for its bottom half and whatever is associated with `5` for its top half.

The new pill content after this step will have the same shape as *s'*:

*c* := `content([[cell(4,`?`),cell(8,`?`)]])`

The bottom half asks for the color associated with `6`. Since `6` appears at row 0, column 1 in *s*, we look at the color in row 0, column 1 of the original pill content *c* and substitute it. That is color `2`, red, so the first ? can be filled in:

*c* := `content([[cell(4,2),cell(8,`?`)]])`

Similarly, the top half asks for the color associated with `5`. Since `5` appears at row 0, column 0 in *s*, we look at the color in row 0, column 0 of the original pill content *c* and substitute it. That is color `3`, yellow, so the second ? can be filled in:

*c* := `content([[cell(4,2),cell(8,3)]])`

This new pill content is a vertical pill with red on its bottom half and yellow on its top half, a clockwise rotation of the original.

Note that while the server is required to use unique colors everywhere in *s*, there is no such restriction on *s'*; it could very well ask for the following step that has no counterpart in existing Dr. Mario implementations:

*c* = `content([[cell(1,3)],[cell(2,2)]])`  
*s* = `shape([[cell(1,5)],[cell(2,5)]])`  
*s'* = `shape([[cell(4,6),cell(8,6)]])`

Everything is the same here as in the previous example except that *s'* asks for `6` twice rather than one `6` and one `5`. The result would be:

*c* := `content([[cell(4,2),cell(8,2)]])`

This is the content of a vertical pill that now has just one color, red, for both halves. There would be no way to recover a pill that is part red, part yellow from this state.

###### Looping steps and terminating motion updates

If the motion update's **bool** is `false()`, a single step is taken, and its status (success or failure), is used as the status of the entire motion update.

If the motion update's **bool** is `true()`, but the **movement rule**'s second field is `false()`, indicating that this motion cannot be automatically repeated, the motion update fails without changing the state.

If the motion update's **bool** is `true()` and the **movement rule**'s second field is `true()`, then the state is repeatedly stepped according to the current rule, until either

* A step fails. In this case, the state from just before the failed step is kept as the new state, and the motion update fails. OR
* A state is repeated, and so stepping has entered an infinite loop. In this case, the state just before the repeated one is kept as the new state, and the motion update succeeds.

If the motion update succeeds, or the motion update fails and the **movement rule**'s third field is `false()`, indicating that failures are not deadly, nothing special happens. The pill remains under player control, and the client SHOULD advance to the next frame at an appropriate time.

If the motion update fails and the **movement rule**'s third field is `true()`, indicating that failures should end control of the pill, the current state is finalized. The client MUST report its movement history to the server, and the server will use the final state to decide what updates, if any, must be made in the larger game state context.
