# un-unicode
Convert unicode characters to their LaTeX counterpart.


## Features

#### Conversions Features

- Consecutive superscript/subscript: `subscriptₜₑₓₜ` ↦ `subscript_{text}`.
- Nested combining characters: `⟹̸̈` ↦ `\not{\ddot{ \implies }}`.
- Support more than one thousand unicode symbols: `⭁` ↦ `\bsimilarleftarrow`.
- Preserves unrecognized unicode: `选定基点 a ∈ 𝒮ⁿ` ↦ `选定基点 a \in \mathcal{S}^{n}`

#### General Features

- Warning for unrecognized unicode and standalone combining characters.
- Minimal dependencies, only depends on `base`, `containers`, `mtl`, and `directory`,
    which are either official or widely-used and well-maintained
- Minimal and readable code base. 

#### Notice 

Haskell uses your locale (which is typically `UTF-8`) when reading documents.
In very rare cases, your input documents might not align with your locale, 
and this difference will cause an error in the program.
We plan to add an option to specify text encoding prior to 1.0 release.

## Build It Yourself

Download the project and [Haskell stack](https://docs.haskellstack.org/en/stable/), 
run `stack build`, and the output will contain the following line:
```
Installing executable un-unicode-exe in <project dir>/un-unicode/.stack-work/install/<platform>/<hash>/<version number>/bin
```
Go to the directory indicated by the build message, and you will find the executable as `un-unicode-exe`.

## Roadmap 

#### Before 1.0

- [ ] unit test.
- [ ] CI integration and build for macOS and Windows.
- [ ] Indicate line and column number in warnings.
- [ ] Remove magic strings in code.
- [ ] Suppress Warnings of a specific kind.
- [ ] Suppress Warning of characters.
- [ ] Upload to hackage (maybe stackage?).
- [ ] Support specify text encoding as an argument.
- [ ] Support font for unicode characters, for example `ℿ` → `\mathbb{\Pi}`.
- [ ] ✨ And more suggested by you! ✨

#### Backlog

These are features that I don't see a huge demand for,
but I could be wrong. 
If you need these features, please open an issue.

- [ ] locale support for warning/error messages?