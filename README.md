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

# Typography

When defining a new term, the term's first use will be written with *italics*. Italics are also used for mathematical variables. Text strings are written in `monospace`. If the text contains strange characters, they may be escaped for clarity by surrounding a codepoint, written in decimal, with `\u` and `\`. For example, `my 🐕 is cute` and `my \u128021\ is cute` represent the same text. If a text contains a backslash followed by a "u", one or the other will always be escaped. Byte strings are written in `monospace` as an even-length sequence of hexadecimal digits surrounded by single quotes. For example, `'0cafe101'` represents a four-byte sequence with the bytes 12, 175, 225, and 1. That sequence will never be written as `'cafe101'`. To avoid ambiguity, all strings beginning with a single quote will have that single quote escaped; for example, `\u39\0cafe101'` represents a 9-character text which could be used to represent a byte sequence. ALL-CAPS PHRASES such as "MUST" and "MUST NOT" are used as described in RFCs 2119 and 8174.

# Data Model

Every message sent with the damascus protocol has a *type* that describes what values the message can take on. The collection of types available has many influences, including JSON and the theory of algebraic types. Each type is associated with five concepts:

1. A *type name* that will be used to refer to that type in the remainder of this document.
2. An *interpretation*, which is a set-theory description of the values associated with that type.
3. A set of human-readable text strings called *abstract values* that will be used to refer to elements of the interpretation in the remaining sections of this document.
4. A set of compact, machine-readable byte strings called *concrete values*, each of which uniquely identifies an element of the interpretation.
5. A separate *patch* type (and function) for edits that describe a way to transform a value.

Type names are given in parentheses after their subsubsection header. Concrete values are represented in CBOR format as described in RFC 8949. The patch type associated with type *T* can be referred to by Δ*T*. The meaning of patches is given by defining a function *patch* ∈ *T*⨯*P* → *T* for each type.

If you have programmed in any modern language, you can almost certainly learn everything you need to know to understand the rest of this document by reading the table of contents for this section.
On a first read, you might want to skip to the next section, and come back for a more careful review once you need to know the details of patches or encoding into bytes.

In the descriptions of abstract values, the term *whitespace* means a text containing only `\u9\` (tab), `\u10\` (newline), or `\u32\` (space). A *comma-separated sequence* refers to text that contains a sequence of some other specified abstract values with commas in between. Comma-separated sequences are allowed to have arbitrarily many preceding commas, trailing commas, and whitespace around the commas. For example, ` ,, , 35,"35"	` and `,35,"35",` are each a comma-separated sequence of abstract values `35` and `"35"`, but `35,,"35"` is not.

The remaining sections of this document will use abstract values exclusively when talking about inhabitants of a type's interpretation. The remaining subsections of this section will occasionally blur the distinction between a type's name and its interpretation.

## Simple Base Types

### Signed 64-bit Integers (i64)

There is just one number type; its interpretation is the set of signed, 64-bit integer values (-2<sup>63</sup> through 2<sup>63</sup>-1 inclusive).

Abstract values are written in decimal or hexadecimal, with an optional sign prefix (assumed positive if missing). For example, `35`, `+35`, `0x23`, and `+0x23` are all abstract number values representing 35, while `-35` and `-0x23` are abstract number values representing -35.

Concrete values use CBOR's number types, major types 0 (positive integers) and 1 (negative integers). Although CBOR can represent larger numbers, messages MUST NOT include values of major type 0 or 1 that fall outside the range -2<sup>63</sup> through 2<sup>63</sup>-1.

A patch is again an i64, to be added with the usual two's complement wrap-around rules:

*patch*(*v*, *p*) = *v*+*p* mod 2<sup>64</sup>

(The mod operation chooses its representative from the range -2<sup>63</sup> through 2<sup>63</sup>-1.)

Particular messages may restrict numbers further, such as by demanding that they fit in an unsigned byte (0 through 255 inclusive). This may mean that patches are forced to be out of the bounds for the type they are modifying; for example, with the unsigned-byte restriction, to change from `255` to `0`, one must use the patch `-255`, not `1`, even though `-255` does not fit in a single unsigned byte. As another example, if the number is restricted to the range `100` through `110`, none of the possible patches fall in the same range as the i64 itself.

### Strings (string)

The string type is interpreted as sequences of Unicode codepoints.

Abstract values are written surrounded by double quotes (with any internal double quotes or backslashes escaped by prefixing a backslash). For example, `"my \u128021\ is cute"` is an abstract string value with 12 codepoints.

Concrete values use CBOR's text string type, major type 3.

An *atomic string edit* is a value of a custom type (see the "Custom Types" subsection below for more details on how to interpret this table).

| Type name | Variant | Tag | Fields | Constraints | Meaning
| --------- | ------- | --- | ------ | ----------- | -------
| atomic string edit
| | `delete`  | `0`
| |           | | i64 | non-negative | the index of the first codepoint deleted
| |           | | i64 | non-negative | the number of codepoints to delete
| | `insert`  | `1`
| |           | | i64 | non-negative | the index where the first new codepoint will appear
| |           | | string | | the text to insert
| | `replace` | `2` | | | a deletion (of the length of the second field) followed by an insertion
| |           | | i64 | non-negative | the index of the first codepoint to replace
| |           | | string | | the new codepoints to write over the old text

All indexing is done by counting codepoints (and not, say, bytes or words used by some specific encoding). The auxiliary function *astrpatch* (short for "atomic string patch") makes the meaning of these precise:

*astrpatch*(*s*, `delete(`*i*`,` *len*`)`) = *s*<sub>0</sub>, ..., *s*<sub>*i*-1</sub>, *s*<sub>*i*+*len*</sub>, ..., *s*<sub>*n*</sub>  
*astrpatch*(*s*, `insert(`*i*`,` *p*`)`) = *s*<sub>0</sub>, ..., *s*<sub>*i*-1</sub>, *p*<sub>0</sub>, ..., *p*<sub>*m*</sub>, *s*<sub>*i*</sub>, ..., *s*<sub>*n*</sub>  
*astrpatch*(*s*, `replace(`*i*`,` *p*`)`) = *s*<sub>0</sub>, ..., *s*<sub>*i*-1</sub>, *p*<sub>0</sub>, ..., *p*<sub>*m*</sub>, *s*<sub>*i*+*m*+1</sub>, ..., *s*<sub>*n*</sub>

In the above equations, the variables *m* and *n* stand in for the lengths of *p* and *s*, respectively. The usual math convention — where out-of-bound indexes are simply dropped from the sequence — applies here. A string patch is a sequence of atomic string edits, to be applied in order.

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
| atomic sequence edit
| | `delete` | `0`
| |          | | i64 | non-negative | the index of the first element deleted
| |          | | i64 | non-negative | the number of elements to delete
| | `insert` | `1`
| |          | | i64 | non-negative | the index where the first new element will appear
| |          | | [*T*] | | the elements to insert
| | `modify` | `2` | | | in-place mutation
| |          | | i64 | non-negative | the index of the first element to modify
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

(The intention is that exactly one row of the above table applies to each key in *K*. If this is not the case, it is a bug in the spec and the maintainer would like to hear about it.)

## Custom Types (various, textual names)

For the algebraically minded, custom types are sums of products with an implicit top-level least fixpoint. Rust programmers will recognize these as enumerations; Haskell programmers as strict data types. Each custom type has a collection of *variants*, and each variant has a sequence of types that serve as its *fields*. As custom types are introduced in this document, they will be presented with a table that looks like this example:

| Type name | Variant | Tag | Fields | Constraints | Meaning
| --------- | ------- | --- | ------ | ----------- | -------
| server step
| | `pill` | `0` | | | The player is being given control of a new pill.
| |        | | i64 | non-negative and smaller than the board width | the x position of the bottom left of a new pill
| |        | | i64 | non-negative and smaller than the board height | the y position of the bottom left of a new pill
| |        | | i64 | non-negative and smaller than the number of available pill shapes | the shape of a new pill, as an index into the list of available pill shapes
| |        | | [i64] | has an appropriate length for the given pill shape | the colors of each part of the pill
| | `new board` | `1` | | | The player must wait while the board changes.
| |             | | [[cell]] | is of size board width ⨯ board height | the cells for a new state of the board

(All custom types in this subsection are for expository purposes only, scoped only to this subsection, and not guaranteed to be consistent with similarly-named types from elsewhere in this document.)

To avoid ambiguity in abstract values, the variant names will not begin or end with whitespace. To avoid ambiguity in concrete values, the variants' tags will be pairwise unequal. All tags are of type i64.

For each variant, we can construct a set of tuples. These tuples have one more element than there are fields, and consist of the variant name followed by an interpretation of each field type. The interpretation of the new custom type is the union of these sets over all variants. Custom types are permitted to recursively mention themselves (or other mutually recursive custom types) as fields; the interpretation uses a least fixed-point construction.

TODO: after nailing down the format of cells, come back and make this `new board` message more interesting (and its followup in the concrete value discussion)

An abstract value is constructed by including the variant name, optional whitespace, and a comma-separated sequence of the abstract values for the fields surrounded by parentheses. For example, `pill(3, 7, 0, [1, 2])` describes a new pill of the default shape coming under control at position (3, 7) with blue and red halves, and `new board([[0, 0], [0, 0]])` describes the player being shown a blank 2⨯2 board.

In the simplest case, a concrete value uses CBOR's array type, major type 4. The array contains a concrete value for a tag followed by concrete values for each of the associated variant's fields. For example, the abstract value `pill(3, 7, 0, [1, 2])` could be encoded to the 8-byte concrete value `'8500030700820102'`, and the abstract value `new board([[0, 0], [0, 0]])` could be encoded to the 9-byte concrete value `'820182820000820000'`.

Two exceptions are made to this pattern. First, if there is exactly one variant, the tag MUST be omitted from the array, yielding an array one element shorter than usual. Second, if the result is an array of exactly one element, the array wrapping mechanism MUST be omitted, causing the concrete value to be that element directly. For example, consider these hypothetical types:

| Type name | Variant | Tag | Fields | Constraints | Meaning
| --------- | ------- | --- | ------ | ----------- | -------
| possible i64
| | `nothing` | `0` | | | no number available
| | `just`    | `1` | | | there is a number available
| |           | | i64
| color
| | `black`  | `0`
| | `blue`   | `1`
| | `red`    | `2`
| | `yellow` | `3`
| shape
| | `shape`  | `0`
| |          | | i64 | fits in an unsigned byte | an index into the sequence of available shapes

The abstract value `just(3)` would use the normal pattern, and so might be encoded to the concrete value `'820103'`, say. But since `nothing()` doesn't have any fields, it would be a 1-element array with just the tag, and so is encoded not as an array but as the tag directly, meaning the concrete value `'00'` would be one possible corresponding concrete value.

Like `nothing()`, the abstract values `black()` and friends have no fields, so the concrete values `'00'`, `'01'`, `'02'`, and `'03'` would be correct encodings of the variants of a color.

Since there is just one variant in the shape type, the abstract value `shape(3)` would not include the tag in its concrete value. Since there is just one field, this means there would be just one element if we were to use an array, and so we simply include the field directly. This means that `03` is an example of a correct concrete value for `shape(3)`. Contrast the concrete value for `just(3)` -- in both cases there is just one field, but with `just(3)` there are other variants and so the tag is not omitted. Contrast also the concrete value for `yellow()`, which is also a bare value and not a CBOR array; however, because the color type has many variants, the bare value must be a tag, and because the shape type has a single variant, the bare value must be a field.

The shape type described above is a case of particular interest. The rules described above mean that although the shape type is notionally distinct from the i64 type, it nevertheless has the same representations in concrete values; that is, there is no overhead associated with introducing extra levels of aliasing in this way.

Patches allow for patching fields within a variant, providing a complete new value (potentially a different variant), or doing nothing. In detail: suppose the type under consideration is *T*. The type Δ*T* is also a custom type. It has one variant for each variant of *T* with at least one field. The new variant name has `Δ` prepended, and the new tag is one bigger. The new variant has as many fields as the old one; their types are the patch types for the old variant's fields. If *T* does not have exactly one variant, then Δ*T* also includes two new variants: `no change`, with no fields and a tag one smaller than the smallest of the other variants of Δ*T* (or `0` if all variants of *T* have zero fields); and `replace`, with a field of type *T* and a tag one larger than the largest of the other variants of Δ*T* (or `1` if all variants of *T* have zero fields). The following table gives examples for each of the custom types previously discussed:

| Type name | Variant | Tag | Fields
| --------- | ------- | --- | ------
| Δserver step
| | `no change` | `0`
| | `Δpill` | `1`
| |         | | Δi64 (=i64)
| |         | | Δi64
| |         | | Δi64
| |         | | Δ[i64]
| | `Δnew board` | `2`
| |              | | Δ[[cell]]
| | `replace` | `3`
| |           | | server step
| Δpossible i64
| | `no change` | `1`
| | `Δjust`     | `2`
| |             | | Δi64
| | `replace`   | `3`
| |             | | possible i64
| Δcolor
| | `no change` | `0`
| | `replace`   | `1`
| |             | | color
| Δshape
| | `Δshape`  | `1`
| |           | | Δi64

As with concrete values, the patches for a custom type that simply wraps another type have no associated overhead; their concrete values are exactly the same as the concrete values for patches to the wrapped type.

This protocol will not use patches for a type if that would cause the `no change` or `replace` tags to be out of the i64 range.

The *patch* implementation does the obvious thing. When given a collection of field patches for the wrong variant, the patch is ignored.

*patch*(*v*, `replace(`*v'*`)`) = *v'*  
*patch*(*V*`(`*x*<sub>0</sub>`,` ...`,` *x*<sub>*n*</sub>`)`, `Δ`*V*`(`*p*<sub>0</sub>`,` ...`,` *p*<sub>*n*</sub>`)`) = *V*`(`*patch*(*x*<sub>0</sub>, *p*<sub>0</sub>), ..., *patch*(*x*<sub>*n*</sub>, *p*<sub>*n*</sub>)`)`  
*patch*(*v*, *p*) = *v*        in all other cases (including when *p* = `no change()`)
