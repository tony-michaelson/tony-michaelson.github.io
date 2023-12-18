---
title: "How to Program in Any Language: Part A"
date: 2023-07-13 00:00:00
featured_image: "/images/articles/program-any-language.png"
excerpt: Focus on concepts not syntax
---

In the vast, intricate cosmos of computer programming, the journey to mastering the art of coding often begins with a profound realization: the syntax of a programming language, while important, is but a mere vessel for the deeper concepts that form the backbone of all programming. This essay is a about transcending the surface-level intricacies of syntax, embracing the universal principles of computer science, and understanding how to select the most suitable language for a given task.

### Focus on Concepts and Intent

Imagine syntax as the varied dialects of a universal language. Whether it's Python's clean, English-like simplicity or Rust's efficiency, each programming language has its unique way of articulating instructions. However, beneath these diverse dialects lies a common, unifying grammar – a set of fundamental concepts that are universal across all programming languages.

Grasping these concepts is akin to an awakening, a realization that regardless of the programming language, you are always engaging with the same core ideas: control structures (if-else, loops), data structures (arrays, lists, trees), algorithms, etc. These are the pillars of programming; learning them is like tapping into the primordial wisdom that underlies all creation in the realm of coding.

### Selecting the Right Language

Choosing a programming language for a project is not merely a technical decision; it's an act of aligning with the essence of the task at hand. It involves understanding the soul of your project and asking: which language best resonates with this endeavor?

| Aspect                             | Considerations                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| ---------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Nature of the Project              | Consider the terrain you're navigating. Is it a web application, a desktop software, or a microcontroller for a robotic arm? Python might be your guide in the world of data science, JavaScript could open the gates of web development, while C might connect you deeply with system programming.                                                                                                                                                                              |
| Performance Needs                  | Just as different journeys require different modes of transport, different projects have varied performance needs. A language like Rust might be the thoroughbred horse for a race, swift and powerful for high-performance needs, whereas Ruby might be a gentle river, perfect for scripts where speed is secondary to ease and flow of writing.                                                                                                                               |
| Language Features                  | The intrinsic characteristics of a language can greatly influence its suitability for a project. For example, Scala's strong typing and object-oriented features make it a reliable choice for large-scale enterprise applications, while Python's dynamic typing and readability are perfect for quick prototyping and scripting. Functional programming languages like Haskell offer advantages in scenarios requiring a high level of abstraction and mathematical precision. |
| Community and Ecosystem            | Some languages are like ancient trees, with deep roots and a vast canopy – these have extensive libraries, frameworks, and community support. For instance, JavaScript's ecosystem is an ever-expanding universe, making it a versatile choice for many applications.                                                                                                                                                                                                            |
| Long-term Maintenance              | Look ahead on the path – will the language stand the test of time for your project? Languages with active communities and ongoing development are like well-trodden paths, easier to maintain and evolve.                                                                                                                                                                                                                                                                        |
| Personal Resonance and Team Skills | Finally, listen to the harmony between you, your team, and the language. A language that resonates with the collective skills and preferences of the team can turn a laborious march into a graceful dance.                                                                                                                                                                                                                                                                      |

### From Syntax to Symphony: Comparing Grammars

As you embark on this journey, remember that learning a new programming language is not starting anew, but rather, it's adding another instrument to your orchestra. Each language allows you to compose the symphony of your project in a unique way, yet the music's essence remains rooted in the universal principles of computer science.

For example, consider the logic of processing a common data structure like a binary tree.

```python
# data structure
class Tree:
    node_id: integer
    left: Tree (or null)
    right: Tree (or null)
    name: string

# convert a Tree to JSON
function tree_to_json(tree):
    if tree is null:
        return null
    else:
        json_tree = {
            "node_id": tree.node_id,
            "name": tree.name,
            "left": tree_to_json(tree.left),
            "right": tree_to_json(tree.right)
        }
        return json_tree
```

and compare this to the logical difference of using tail-recursion:

```python
# data structure
class Tree:
    node_id: integer
    left: Tree (or null)
    right: Tree (or null)
    name: string

# convert a Tree to JSON
def tree_to_json(tree):
    if tree is None:
        return None
    return {
        "node_id": tree.node_id,
        "name": tree.name,
        "left": tree_to_json(tree.left),
        "right": tree_to_json(tree.right)
    }
```

The important element is understanding the logic required to transform the data structure from a Tree to a valid JSON string and doing this in a way that is efficient no matter how large the data input is. If we do not understand the concept of tail-recursion we're going to ship code that will likely plague our users with stack overflow errors, slow performance, or even worse, cause processing from one user of the system to impact others.

Programming is about expressing your intent to a computer system and knowing the methods of implementation well enough to design a good overall system.

Once you practice and understand the fundamental concepts in programming you become language agnostic. Each language may show you new concepts but you just add them to your toolbelt and look back on all the other languages you've learned to see how they might be expressed in those languages now that you're aware of something new.

Now let's take a look at how we might implement our sudo code in some different languages.

> Scala

```scala
import play.api.libs.json._

case class Tree(
    node_id: Long,
    left: Option[Tree],
    right: Option[Tree],
    name: String
)

def treeToJson(tree: Tree): JsObject = {
Json.obj(
    "node_id" -> tree.node_id,
    "name" -> tree.name,
    "left" -> tree.left.map(treeToJson),
    "right" -> tree.right.map(treeToJson)
    )
}
```

### Usage:

```scala
val myTree: Tree = Tree(
    node_id = 1,
    left = Some(
    Tree(
        node_id = 2,
        left = Some(Tree(4, None, None, "Node 4")),
        right = Some(Tree(5, None, None, "Node 5")),
        name = "Node 2"
    )
    ),
    right = Some(
    Tree(
        node_id = 3,
        left = Some(Tree(6, None, None, "Node 6")),
        right = Some(Tree(7, None, None, "Node 7")),
        name = "Node 3"
    )
    ),
    name = "Node 1"
)

treeToJson(myTree)
```

This code in Scala is an expression of our intent we communicated in our psudocode but to make it a bit more Scala-esk we're going to want to implement this in a more idiomatic way to this language.

> Better Scala

```scala
package com.example

import play.api.libs.json._

case class Tree(
                 node_id: Long,
                 left: Option[Tree],
                 right: Option[Tree],
                 name: String
               ) {
  def toJson: JsObject = {
    Json.obj(
      "node_id" -> node_id,
      "name" -> name,
      "left" -> left.map(_.toJson),
      "right" -> right.map(_.toJson)
    )
  }
}
```

### usage:

```scala
val tree: Tree = Tree(
    node_id = 1,
    left = Some(
    Tree(
        node_id = 2,
        left = Some(Tree(4, None, None, "Node 4")),
        right = Some(Tree(5, None, None, "Node 5")),
        "Node 2"
    )
    ),
    right = Some(
    Tree(
        node_id = 3,
        left = Some(Tree(6, None, None, "Node 6")),
        right = Some(Tree(7, None, None, "Node 7")),
        "Node 3"
    )
    ),
    "Node 1"
)

println(Json.prettyPrint(tree.toJson))
```

You can see we've adapted the expression of our intent to fit with the idiomatic way of the Scala language. Each language has these patterns and we must learn them because not only can we take advantage of some performance improvements in how our code might be implemented by different languages but we are going to make programmers within the eco-system happier when they work with us.

If you'd like to see how this Scala code looks in a normal project structure look here: [https://github.com/tony-michaelson/scala-btree-example](https://github.com/tony-michaelson/scala-btree-example)

> Lang Tour

Let's take a quick tour of some more languages starting with the common to more esoteric.

We'll take a look at how the syntax features being used become more complex as the languages offer more features.

> Javascript

```javascript
function treeToJson(tree) {
  return {
    node_id: tree.node_id,
    name: tree.name,
    left: tree.left ? treeToJson(tree.left) : null,
    right: tree.right ? treeToJson(tree.right) : null,
  };
}
```

### Syntax Features Used

- Function declaration and definition
- Object literal notation
- Conditional (ternary) operator
- Dot notation for object property access
- Parentheses for function calls
- Curly braces `{}` to define code blocks
- Semicolons `;` to terminate statements

> Typescript

```typescript
interface TreeNode {
  node_id: number;
  left?: TreeNode;
  right?: TreeNode;
  name: string;
}

function treeToJson(tree: TreeNode): any {
  return {
    node_id: tree.node_id,
    name: tree.name,
    left: tree.left ? treeToJson(tree.left) : null,
    right: tree.right ? treeToJson(tree.right) : null,
  };
}
```

### Syntax Features Used:

- Interface declaration
- Type annotations (`: number`, `: TreeNode`, `: string`, `: any`)
- Function declaration and definition syntax
- Object literal notation
- Conditional (ternary) operator syntax
- Optional properties (`left?: TreeNode`, `right?: TreeNode`)
- Function parameter declaration (`tree: TreeNode`)
- Arrow function syntax (`(tree: TreeNode) => any`)
- Object property access syntax (`tree.node_id`, `tree.name`, `tree.left`, `tree.right`)
- Conditional statements (`if` and `? :` for conditional checks)
- Recursive function call (`treeToJson` calling itself recursively)
- Null (`null`) value
- TypeScript module system (assuming this code is part of a TypeScript module)

> Dart

```dart
class Tree {
  final int node_id;
  final Tree? left;
  final Tree? right;
  final String name;

  Tree({
    required this.node_id,
    this.left,
    this.right,
    required this.name,
  });

  Map<String, dynamic> toJson() {
    return {
      'node_id': node_id,
      'name': name,
      'left': left?.toJson(),
      'right': right?.toJson(),
    };
  }
}
```

### Syntax Features Used:

- Class declaration (`class Tree`)
- Field declarations with final keyword (`final int node_id`, `final Tree? left`, `final Tree? right`, `final String name`)
- Constructor declaration (`Tree({ ... })`)
- Named constructor parameters (`required this.node_id`, `this.left`, `this.right`, `required this.name`)
- Map data structure (`Map<String, dynamic>`)
- Method declaration (`Map<String, dynamic> toJson()`)
- Object initialization (`{ ... }`)
- Conditional property access (`left?.toJson()`, `right?.toJson()`)
- Dart type system (e.g., `int`, `Tree?`, `String`)
- Null safety annotations (`?` for nullable types)
- Dart's named argument syntax (`Tree({ ... })`)
- Return statement (`return { ... }`)

> C

```c
#include <stdio.h>
#include <stdlib.h>

struct TreeNode {
    long node_id;
    struct TreeNode* left;
    struct TreeNode* right;
    char name[50];
};

struct TreeNode* createTreeNode(long node_id, char name[50]) {
    struct TreeNode* newNode = (struct TreeNode*)malloc(sizeof(struct TreeNode));
    newNode->node_id = node_id;
    newNode->left = NULL;
    newNode->right = NULL;
    strcpy(newNode->name, name);
    return newNode;
}
```

### Syntax Features Used:

- Preprocessor directives (`#include <stdio.h>`, `#include <stdlib.h>`)
- Structure declaration (`struct TreeNode`)
- Pointer declaration (`struct TreeNode*`)
- Function declaration and definition (`struct TreeNode* createTreeNode(long node_id, char name[50])`)
- Dynamic memory allocation (`malloc`)
- Type casting (`(struct TreeNode*)`)
- Assignment (`newNode->node_id = node_id`, `newNode->left = NULL`, `newNode->right = NULL`)
- String manipulation (`strcpy(newNode->name, name)`)
- Return statement (`return newNode;`)
- Standard C library functions (`malloc`, `strcpy`)
- Data types (`long`, `char`)
- Function parameters (e.g., `long node_id`, `char name[50]`)
- Null pointer assignment (`NULL`)

> Rust

```rust
use serde::Serialize;
use serde_json::json;
use serde_json::Value;

#[derive(Serialize)]
struct Tree {
    node_id: i64,
    left: Option<Box<Tree>>,
    right: Option<Box<Tree>>,
    name: String,
}

fn tree_to_json(tree: &Tree) -> Value {
    json!({
        "node_id": tree.node_id,
        "name": &tree.name,
        "left": tree.left.as_ref().map(|left| tree_to_json(left)),
        "right": tree.right.as_ref().map(|right| tree_to_json(right)),
    })
}
```

### Syntax Features Used:

- Rust module system (`use serde::Serialize`, `use serde_json::json`, `use serde_json::Value`)
- Struct definition (`struct Tree { ... }`)
- Deriving traits (`#[derive(Serialize)]`)
- Primitive data types (`i64`, `String`)
- Option type (`Option<Box<Tree>>`)
- Box smart pointer (`Box<Tree>`)
- Function declaration and definition (`fn tree_to_json(tree: &Tree) -> Value { ... }`)
- Function parameters (`tree: &Tree`)
- References (`&`)
- Method chaining (`tree.left.as_ref().map(...)`)
- Closures (`|left| tree_to_json(left))`)
- JSON serialization (`json!({ ... })`)
- Rust's ownership and borrowing system
- Struct field access (`tree.node_id`, `tree.name`, `tree.left`, `tree.right`)

> Erlang

```elixir
-module(tree).
-compile(export_all).

-record(tree, {node_id, left, right, name}).

create_node(NodeId, Left, Right, Name) ->
    #tree{node_id=NodeId, left=Left, right=Right, name=Name}.

tree_to_json(Tree) ->
    {struct, [
        {<<"node_id">>, Tree#tree.node_id},
        {<<"name">>, Tree#tree.name},
        {<<"left">>, tree_to_json(Tree#tree.left)},
        {<<"right">>, tree_to_json(Tree#tree.right)}
    ]}.
```

### Syntax Features Used:

- Module declaration (`-module(tree).`)
- Compilation directive (`-compile(export_all).`)
- Record definition (`-record(tree, {node_id, left, right, name}).`)
- Function definition (`create_node(NodeId, Left, Right, Name) -> ...`)
- Pattern matching (`#tree{...}`)
- Tuple construction (`{struct, [...]}`)
- Atoms (e.g., `<<"node_id">>`)
- Function parameters (e.g., `NodeId`, `Left`, `Right`, `Name`)
- Recursive function (`tree_to_json(Tree) -> ...`)
- Function clauses (`tree_to_json(Tree) -> ...`)
- Field access (`Tree#tree.node_id`, `Tree#tree.name`, `Tree#tree.left`, `Tree#tree.right`)

> Racket

```lisp
(define-struct tree (node-id left right name))

(define (create-node node-id left right name)
  (make-tree node-id left right name))

(define (tree-to-json tree)
  (if tree
      (let ((left-json (if (tree-left tree)
                           (tree-to-json (tree-left tree))
                           #f))
            (right-json (if (tree-right tree)
                            (tree-to-json (tree-right tree))
                            #f)))
        (list (cons "node_id" (tree-node-id tree))
              (cons "name" (tree-name tree))
              (cons "left" left-json)
              (cons "right" right-json)))
      #f))
```

### Syntax Features Used:

- Struct definition (`(define-struct tree (node-id left right name))`)
- Function definition (`(define (create-node node-id left right name) ...)`)
- Function parameters (e.g., `node-id`, `left`, `right`, `name`)
- Structure constructor (`(make-tree node-id left right name)`)
- Conditional statement (`(if ... ...)`)
- Local variable binding (`(let (...) ...)`)
- Lists and list construction (`(list ...)`)
- Association lists (`(cons ... ...)`)
- Predicates (`tree`, `tree-left`, `tree-right`)
- Converting between types (e.g., `(tree-node-id tree)`)
- Null value (`#f`)

> Standard ML

```sml
datatype TreeNode = TreeNode of {
  node_id: int,
  left: TreeNode option,
  right: TreeNode option,
  name: string
};

fun treeToJson (tree: TreeNode): TreeNode option =
  case tree of
    TreeNode{node_id, left, right, name} =>
      SOME(TreeNode{
        node_id = node_id,
        left = (case left of
                 SOME leftTree => treeToJson leftTree
               | NONE => NONE),
        right = (case right of
                  SOME rightTree => treeToJson rightTree
                | NONE => NONE),
        name = name
      });
```

### Syntax Features Used:

- Datatype declaration (`datatype TreeNode = TreeNode of { ... };`)
- Function definition (`fun treeToJson (tree: TreeNode): TreeNode option = ...`)
- Pattern matching (`case tree of ...`)
- Record pattern (`TreeNode{node_id, left, right, name} => ...`)
- Constructor usage (`SOME(TreeNode{ ... })`)
- Recursive function (`treeToJson` calling itself recursively)
- Option type (`TreeNode option`)
- Null value (`NONE`)
- Function parameters (`tree: TreeNode`)
- Type annotations (`(tree: TreeNode): TreeNode option`)

> Haskell

```haskell
data TreeNode = TreeNode {
    node_id :: Int,
    left :: Maybe TreeNode,
    right :: Maybe TreeNode,
    name :: String
  } deriving Show

treeToJson :: TreeNode -> Maybe TreeNode
treeToJson tree =
  case tree of
    TreeNode { node_id = nodeId, left = leftTree, right = rightTree, name = nodeName } ->
      Just TreeNode
        { node_id = nodeId
        , left = case leftTree of
                   Just leftNode -> treeToJson leftNode
                   Nothing -> Nothing
        , right = case rightTree of
                    Just rightNode -> treeToJson rightNode
                    Nothing -> Nothing
        , name = nodeName
        }
```

### Syntax Features Used:

- Data type declaration (`data TreeNode = TreeNode { ... } deriving Show`)
- Record syntax for data types (`node_id :: Int`, `left :: Maybe TreeNode`, `right :: Maybe TreeNode`, `name :: String`)
- Type annotations (`:: TreeNode -> Maybe TreeNode`)
- Function definition (`treeToJson tree = ...`)
- Pattern matching (`case tree of ...`)
- Named field pattern matching (`TreeNode { node_id = nodeId, left = leftTree, right = rightTree, name = nodeName } -> ...`)
- Data constructor usage (`Just TreeNode { ... }`)
- Option type (`Maybe TreeNode`)
- Recursive function (`treeToJson` calling itself recursively)
- Function parameters (`tree: TreeNode`)
- Haskell's type inference and type system

### Conclusion

As you can see, the language features can become rather complex depending on what we're going to express our psudocode in! The art of learning languages is all in understanding the nuances of each target language. Once you understand the underlying concepts learing how they are expressed in any language is a secondary process.

It should only take about five programming languages to give you a layout of the landscape but I recommend a list more like:

- **Python**: For high-level scripting, general-purpose programming, and ease of learning.
- **JavaScript**: Essential for web development (both front-end and back-end with Node.js).
- **Java**: Representing strongly-typed, object-oriented programming, widely used in enterprise environments.
- **C**: For understanding low-level programming, memory management, and systems programming.
- **Haskell**: As a representative of pure functional programming paradigms.
- **SQL**: For database management and data manipulation.
- **Rust**: Covering modern systems programming with an emphasis on safety and concurrency.
- **Kotlin or Swift**: For modern mobile app development (Kotlin for Android, Swift for iOS).

### Topics for Learning a New Language

These are the primary topics I believe one should cover when learning a new language.

| Topic                               | Description                                                                                                               |
| ----------------------------------- | ------------------------------------------------------------------------------------------------------------------------- |
| **Build Tooling**                   | Familiarize yourself with the build tools and package managers specific to the language for efficient project management. |
| **Frameworks**                      | Explore the most popular or relevant frameworks that facilitate rapid and structured development in the language.         |
| **Language Features & Style Guide** | Understand unique language features and adopt standard coding styles for readability and maintainability.                 |
| **Idioms**                          | Grasp the common idiomatic expressions in the language to write more efficient, readable, and 'native' code.              |
| **System Libraries**                | Get to know the standard libraries provided by the language for various tasks like networking, file I/O, etc.             |
| **Community and Ecosystem**         | Engage with the language's community for support, and explore its ecosystem for resources, libraries, and tools.          |

And then cover:

| Topic                          | Description                                                                                                        |
| ------------------------------ | ------------------------------------------------------------------------------------------------------------------ |
| **Testing & Debugging**        | Learn the standard practices for testing and debugging in the language to ensure code reliability and quality.     |
| **Concurrency & Parallelism**  | Explore how the language handles concurrent and parallel programming, essential for high-performance applications. |
| **Cross-platform Development** | Learn about the language's capability for cross-platform development and any platform-specific considerations.     |
| **Integration Capabilities**   | Understand how to integrate the language with other systems, languages, or frameworks.                             |
