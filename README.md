# Nexity 
~ another compiler flavor to the Haskell language

## Motivation
The idea to do a Rust-based parser came from a compiler club I joined at my university, where I was fascinated by the idea of compilers and the LLVM build system. Part of that comes from when I was doing my research for dynamic code analysis tools. In that, I learned the four fundamental stages of parsing (lexer -> parser -> semantic analysis -> code generator). While there wasn't much done on the project, after taking Programming Language Paradigms, now I have more awareness of how I want to tackle this unique challenge. 

## How it Works
### Lexer
The lexer transforms Haskell code into a stream of `Token` that're helpful for identifying numbers, operators, and reserved keywords of the Haskell language specification. The lexer is made, for now, using a stateful, character-by-character lexer. The things it supports:
- Identifiers (`let`, `x`, `sum`, etc.)
- Reserved keywords (`case`, `data`, `do`, `if`, `then`, `else`, …)
- Numeric literals (e.g., 10, 3.14)
- Single-character operators (+, -, *, /, %, =, |, etc.)
- Comments (# … until newline)
- Parentheses for grouping

For example, take this expression: 
```
10 + 20 * 3 % 5
```

It will transform to 
```
Number(10.0)
Char('+')
Number(20.0)
Char('*')
Number(3.0)
Char('%')
Number(5.0)
Eof
```

## Parser
This parser implements the top-down approach rather than LR parsers or recursive-descent, which would need to consume a full grammar for each precedence level (felt it may take up overhead). This Pratt parser is how this compiler will take those token streams (seen above) and converges to an abstract syntax tree (AST). Later, I'm hoping on building the Hindley-Milner type inference system (very useful in contexts to functional programming languages, like Haskell). Here are things my parser supports:
- Arithmetic expressions with proper operator precedence (+, -, *, /, %)
- Parenthesized grouping ((expr))
- Identifiers and function application
- Lambda expressions (\x -> expr)
- Function definitions (f x = expr)
- Pipe operators (| func | arg)

Credits to: https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html for a nice read on how the Hindley-Milner (Algorithm W) works. Note: taking programming language paradigms will help you understand most of what the article talks about.

### Architecture Summary
This is the flow of Nexity (as a state machine):
<img width="1327" height="455" alt="image" src="https://github.com/user-attachments/assets/c838ffb0-1e90-44d7-88b2-5376360106ea" />


### Design Notes
- The lexer uses the `Arc<Mutex>` idiom to help share the same resource across all threads so that we have proper synchronization and mutability
- The parser follows the **Pratt Algorithm** which naturally will handle operator precedence and associativity in a clean, recursive form. Later implementation will include the Hindley-Milner type system after I get the AST successfully made
