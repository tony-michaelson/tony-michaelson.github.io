---
title: "Concise Rust"
date: 2024-01-01 00:00:00
featured_image: "/images/articles/m/concise-rust.png"
excerpt: A concise language guide for getting productive in Rust as quickly as possible.
---

### Introduction

Rust is a modern, multi-paradigm programming language known for its focus on safety and performance. Unique in its approach, Rust offers memory safety guarantees through its ownership model without relying on a garbage collector. This distinctive feature enables developers to write low-level code that is both efficient and memory-safe.

### Key Characteristics

| Feature                  | Description                                                                                                                 |
| ------------------------ | --------------------------------------------------------------------------------------------------------------------------- |
| Memory Safety            | The borrow checker enforces memory safety at compile time, eliminating common bugs like null pointers and buffer overflows. |
| Concurrency Without Fear | The ownership model naturally prevents data races, making concurrent programming more approachable and less error-prone.    |
| Zero-Cost Abstractions   | Provides high-level abstractions without sacrificing performance, often matching or even surpassing C/C++ in speed.         |
| Eco-System and Tooling   | Offers robust tooling, including Cargo for package management, and a growing ecosystem of libraries and frameworks.         |
| Cross-Platform Support   | Supports a wide range of platforms, from embedded systems to WebAssembly.                                                   |

### Primary Use Cases

You can arrange the additional information into a Markdown table like this:

| Use Case                      | Description                                                                                                                                                                          |
| ----------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Systems Programming           | Ideal for building operating systems, file systems, and other systems-level components due to its speed and safety.                                                                  |
| Web Assembly                  | Efficient performance and safe concurrency make it a popular choice for WebAssembly, enhancing web applications.                                                                     |
| Embedded Systems              | Its low overhead and cross-platform support make it suitable for embedded systems development.                                                                                       |
| Network Programming           | Used in creating fast and reliable network services and applications.                                                                                                                |
| Cryptocurrency and Blockchain | Security features and performance efficiency make it a favored language for developing blockchain technologies and cryptocurrency platforms, where security and speed are paramount. |

### Install

[https://www.rust-lang.org/tools/install](https://www.rust-lang.org/tools/install)

### Project Structure

Rust projects are built by `cargo` and you can create a new project with `cargo new ...` and it will create a project structure like:

```
my_rust_project/
  ├── src/
  │   ├── main.rs        # The main entry point of your program
  │   └── lib.rs         # Module file for library crates
  ├── Cargo.toml         # Manifest file for package metadata and dependencies
  └── README.md          # Documentation and project description
```

A more typical Rust project will look like:

```
my_rust_project/
  ├── src/
  │   ├── main.rs
  │   ├── lib.rs
  │   ├── module1.rs
  │   └── module2.rs
  ├── Cargo.toml
  ├── README.md
  ├── tests/
  │   ├── integration_tests.rs
  │   └── unit_tests.rs
  ├── examples/
  │   ├── example1.rs
  │   └── example2.rs
  ├── docs/
  │   ├── index.html
  │   └── ...
  └── .gitignore
```

### Build

Use `cargo build` to build the source to the `/target` directory. Use `cargo run` to both build and execute the program. The name of the compiled program will match the `name` defined in your `Cargo.toml` file.

### Libraries

Libraries of code are called `crates` and you can search the public repository at [https://crates.io/](https://crates.io/)

### Entrypoint

```rust
// in main.rs
fn main() {
    ...
}
```

### Data Types

**Scalar Types:** These represent single values.

| Data Type | Description                             | Example Values                                              | Size (in bits)               | Range or Precision        |
| --------- | --------------------------------------- | ----------------------------------------------------------- | ---------------------------- | ------------------------- |
| `i8`      | 8-bit signed integer                    | `-128` to `127`                                             | 8 bits                       | -                         |
| `i16`     | 16-bit signed integer                   | `-32,768` to `32,767`                                       | 16 bits                      | -                         |
| `i32`     | 32-bit signed integer                   | `-2,147,483,648` to `2,147,483,647`                         | 32 bits                      | -                         |
| `i64`     | 64-bit signed integer                   | `-9,223,372,036,854,775,808` to `9,223,372,036,854,775,807` | 64 bits                      | -                         |
| `i128`    | 128-bit signed integer                  | Large range                                                 | 128 bits                     | -                         |
| `isize`   | Platform-dependent signed integer       | Platform-dependent range                                    | Platform-dependent size      | -                         |
| `u8`      | 8-bit unsigned integer                  | `0` to `255`                                                | 8 bits                       | -                         |
| `u16`     | 16-bit unsigned integer                 | `0` to `65,535`                                             | 16 bits                      | -                         |
| `u32`     | 32-bit unsigned integer                 | `0` to `4,294,967,295`                                      | 32 bits                      | -                         |
| `u64`     | 64-bit unsigned integer                 | `0` to `18,446,744,073,709,551,615`                         | 64 bits                      | -                         |
| `u128`    | 128-bit unsigned integer                | Large range                                                 | 128 bits                     | -                         |
| `usize`   | Platform-dependent unsigned integer     | Platform-dependent range                                    | Platform-dependent size      | -                         |
| `f32`     | 32-bit floating-point number (IEEE 754) | Varies                                                      | 32 bits                      | Approx. 7 decimal digits  |
| `f64`     | 64-bit floating-point number (IEEE 754) | Varies                                                      | 64 bits                      | Approx. 15 decimal digits |
| `char`    | A single Unicode character              | `'a'`, `'🚀'`, `'日'`                                       | 32 bits (Unicode code point) | -                         |
| `bool`    | Represents true or false                | `true`, `false`                                             | 1 bit                        | -                         |

**Compound Types:** These can group multiple values into one type.

| Data Type      | Description                          | Example Values    |
| -------------- | ------------------------------------ | ----------------- |
| `tuple`        | Ordered, fixed-size collection       | `("Rust", 2022)`  |
| `array`        | Fixed-size array                     | `[1, 2, 3]`       |
| `slice`        | Dynamically-sized view into an array | `[1, 2, 3][1..3]` |
| `str` (string) | String slices                        | `"Hello, Rust!"`  |
| `Vec` (vector) | Dynamic array                        | `vec![1, 2, 3]`   |

**Reference Types:** These are used to borrow values without transferring ownership.

| Data Type         | Description         | Example      |
| ----------------- | ------------------- | ------------ |
| `&T` (references) | Immutable reference | `&x`, `&str` |
| `&mut T`          | Mutable reference   | `&mut y`     |

**Ownership Types:** These manage memory and resource ownership.

| Data Type | Description                   | Example                   |
| --------- | ----------------------------- | ------------------------- |
| `String`  | Owned, heap-allocated strings | `String::from("Rust")`    |
| `Box<T>`  | Owned, heap-allocated data    | `Box::new(42)`            |
| `Rc<T>`   | Reference-counted data        | `Rc::new(vec![1, 2, 3])`  |
| `Arc<T>`  | Atomic reference-counted data | `Arc::new(vec![1, 2, 3])` |

### Basic Syntax

Rust uses a C-style syntax and we're expected to use [snake casing](https://en.wikipedia.org/wiki/Snake_case) for our variable and function names.

```rust
fn my_function(x: u8) {
    println!("X: {x}") // no semi-colon
}

fn main() {
    my_function(5); // manditory statement termination using ;
    my_function(6)
}
```

### Macros

`println` is a macro which is defined as:

```rust
#[macro_export]
#[stable(feature = "rust1", since = "1.0.0")]
#[cfg_attr(not(test), rustc_diagnostic_item = "println_macro")]
#[allow_internal_unstable(print_internals, format_args_nl)]
macro_rules! println {
    () => {
        $crate::print!("\n")
    };
    ($($arg:tt)*) => {
        $crate::io::_print(
            $crate::format_args_nl!($($arg)*)
        );
    };
}
```

Macros are called using the syntax `macro_name!` and we are told:

```rust
/// Use `println!` only for the primary output of your program. Use
/// [`eprintln!`] instead to print error and progress messages.
```

Which I have yet to see anyone do!

### Variable Binding

- use either let or const
- it supports type inferencing
- you can shadow
- immutable by default
- add `mut` to define a mutable variable
- {} braces are scopes
- closures exist

```rust
const DAYS_IN_A_YEAR: u16 = 365; // constant with static type

fn my_function(x: u16) { // scope
    println!("X: {x}")
}

fn main() { // scope
let x = 0; // immutable variable with inferenced type
    { // scope
        let x = DAYS_IN_A_YEAR - 1; // shadow
        my_function(x);
    }
}
```

A closure that captures `x`

```rust
let x = 10;
let add_x = |y| { // closed with x in scope
    x + y
};
let result = add_x(5);
```

And result is still here `15`

```rust
let x = 10;
let add_x = |y| {
    x + y
};
let x = 44;
let result = add_x(5);
```

Using different capture modes

```rust
let mut x = 5;
let increment = || {
    x += 1;
};
let read_x = || {
    println!("x is: {}", x);
};
increment(); // Captures `x` mutably and increments it
read_x();    // Captures `x` immutably and reads it
```

### Operators

Numeric operators are standard

```rust
fn main() {
    // Arithmetic operators
    let a = 10;
    let b = 5;

    let addition = a + b;
    let subtraction = a - b;
    let multiplication = a * b;
    let division = a / b;
    let remainder = a % b;

    println!("Addition: {} + {} = {}", a, b, addition);
    println!("Subtraction: {} - {} = {}", a, b, subtraction);
    println!("Multiplication: {} * {} = {}", a, b, multiplication);
    println!("Division: {} / {} = {}", a, b, division);
    println!("Remainder: {} % {} = {}", a, b, remainder);

    // Increment and Decrement
    let mut x = 5;
    x += 1; // Increment
    println!("Increment x by 1: {}", x);

    let mut y = 10;
    y -= 1; // Decrement
    println!("Decrement y by 1: {}", y);

    // Floating-point arithmetic
    let c = 3.5;
    let d = 2.0;

    let float_addition = c + d;
    let float_subtraction = c - d;
    let float_multiplication = c * d;
    let float_division = c / d;

    println!("Floating-point Addition: {} + {} = {}", c, d, float_addition);
    println!("Floating-point Subtraction: {} - {} = {}", c, d, float_subtraction);
    println!("Floating-point Multiplication: {} * {} = {}", c, d, float_multiplication);
    println!("Floating-point Division: {} / {} = {}", c, d, float_division);
}

```

### Control Flow

- We have beautiful pattern matching, yay!!!
- While, for, if/else, and loop look standard

```rust
fn main() {
    // Conditional statements with if and else
    let number = 10;

    if number < 5 {
        println!("Number is less than 5");
    } else if number == 5 {
        println!("Number is equal to 5");
    } else {
        println!("Number is greater than 5");
    }

    // Matching values with match
    let day = "Wednesday";

    match day {
        "Monday" => println!("It's Monday!"),
        "Tuesday" => println!("It's Tuesday!"),
        "Wednesday" => println!("It's Wednesday!"),
        "Thursday" => println!("It's Thursday!"),
        "Friday" => println!("It's Friday!"),
        _ => println!("It's the weekend!"),
    }

    // Loops
    let mut count = 0;

    // While loop
    while count < 5 {
        println!("While loop: Count is {}", count);
        count += 1;
    }

    // For loop using a range
    for number in 1..6 {
        println!("For loop: Number is {}", number);
    }

    // For loop using an iterator
    let fruits = ["apple", "banana", "cherry", "date"];

    for fruit in fruits.iter() {
        println!("For loop (iterator): Fruit is {}", fruit);
    }

    // Loop with break and continue
    let mut i = 0;

    loop {
        if i == 3 {
            println!("Breaking the loop");
            break;
        }
        println!("Loop iteration: {}", i);
        i += 1;
    }
}

```

A more advanced pattern matching example

```rust
enum Animal {
    Cat { name: String, age: u8 },
    Dog { name: String, breed: String },
    Bird(String),
}

fn main() {
    let my_pet = Animal::Cat {
        name: String::from("Whiskers"),
        age: 3,
    };

    match my_pet {
        Animal::Cat { name, age } => { // pattern guards
            println!("I have a cat named {} who is {} years old.", name, age);
        }
        Animal::Dog { name, breed } => {
            println!("I have a dog named {} of breed {}.", name, breed);
        }
        Animal::Bird(name) if name == "Robin" => {
            println!("I have a special bird named Robin.");
        }
        Animal::Bird(name) => {
            println!("I have a bird named {}.", name);
        }
    }
}

```

Pattern matching with destructuring

```rust
let point = (10, 20);

match point {
    (0, 0) => println!("Origin"),
    (_, 0) => println!("On the x-axis"),
    (0, _) => println!("On the y-axis"),
    (x, y) => println!("At ({}, {})", x, y),
}
```

### Using Compound Types

**Tuples:**

```rust
let person = ("Alice", 30);
let coordinates = (10, 20, 30);
let empty_tuple = ();
```

**Arrays:**
Arrays are fixed-size, homogeneous collections of values.

```rust
let numbers = [1, 2, 3, 4, 5];
let colors = ["red", "green", "blue"];
```

**Slices:**
Slices are references to a contiguous sequence of elements in an array or other data structures. They are used to borrow a portion of a collection.

```rust
let numbers = [1, 2, 3, 4, 5];
let slice = &numbers[1..4]; // Borrow elements 2, 3, and 4 (more on borrowing in a bit)
```

**String Slices (`str`):**
String slices represent references to portions of strings. They are commonly used for string manipulation.

```rust
let greeting = "Hello, Rust!";
let slice = &greeting[0..5]; // Borrow "Hello"
```

**Vectors (`Vec`):**
Vectors are dynamic, growable arrays. They are defined using the `Vec` type.

```rust
let numbers = vec![1, 2, 3, 4, 5];
```

**Structs:**
Structs allow you to define custom data types with named fields. They are used to create more complex data structures.

```rust
struct Person {
    name: String,
    age: u32,
}

let alice = Person {
    name: String::from("Alice"),
    age: 30,
};
```

**Enums:**
Enums allow you to define a type that can have multiple variants, each with its own data. Enums are used to represent different states or options.

```rust
enum Color {
    Red,
    Green,
    Blue,
}

let selected_color = Color::Red;
```

### Functions

Return types are after `->` and the `return` keyword is not required.

```rust
fn add(x: i32, y: i32) -> i32 {
    x + y // return keyword is optional
}
let result = add(5, 3);
```

Sometimes you may need to inline your functions. Inlining recursive functions can eliminate the overhead of repeated function calls and potentially improve performance for small and simple recursive functions.

```rust
#[inline]
fn factorial(n: u64) -> u64 {
    match n {
        0 | 1 => 1,
        _ => n * factorial(n - 1),
    }
}

fn main() {
    let result = factorial(5);
    println!("Factorial of 5 is: {}", result);
}

```

### Ownership

Rust's concept of ownership is a key part of its memory management system that ensures memory safety and prevents data races.

These are the main points:

- Each Value Has a Single Owner
- Ownership is Exclusive
- Ownership Has Scope
- Ownership Can Be Transferred
- Cloning Creates Independent Copies
- Borrowing and References
- No Simultaneous Mutable References
- No Mutable and Immutable References Together
- Lifetime Annotations

### Ownership Examples

Allocation and deallocation

```rust
fn main() {
    let s = String::from("hello"); // 's' is the owner of the String

    // 's' is in scope and can be used here

} // 's' goes out of scope and the String is automatically deallocated
```

Changing ownership

```rust
fn main() {
    let s1 = String::from("hello");
    let s2 = s1; // Ownership is moved from 's1' to 's2'

    // 's1' cannot be used here because it no longer owns the String

    println!("{}", s2); // 's2' can be used here
}
```

Cloning

```rust
fn main() {
    let s1 = String::from("hello");
    let s2 = s1.clone(); // Creates a new String owned by 's2'

    // Both 's1' and 's2' can be used independently here

    println!("{}", s1);
    println!("{}", s2);
}
```

Cloning a mutable variable.

```rust
fn main() {
    let mut original_vec = vec![1, 2, 3, 4, 5]; // Create a mutable vector

    // Clone the mutable vector
    let mut cloned_vec = original_vec.clone();

    // Modify the original vector
    original_vec.push(6);

    // Modify the cloned vector
    cloned_vec.push(7);

    println!("Original vector: {:?}", original_vec);
    println!("Cloned vector: {:?}", cloned_vec);
}

// output:
// Original vector: [1, 2, 3, 4, 5, 6]
// Cloned vector: [1, 2, 3, 4, 5, 7] // no 6, SEE!
```

You can reference and dereference variables

```rust
fn main() {
    let value = 42;

    // Create an immutable reference
    let reference = &value;

    println!("Value: {}", value);
    println!("Reference: {}", *reference); // Dereference to access the value
}
```

Mutable references

```rust
fn main() {
    let mut value = 42;

    // Create a mutable reference
    let reference = &mut value;

    println!("Value before modification: {}", *reference);

    // Modify the value through the mutable reference
    *reference += 10;

    println!("Value after modification: {}", *reference);
}
```

You cannot have both mutable and immutable references to the same data simultaneously.

```rust
fn main() {
    let mut value = 42;

    // Create an immutable reference
    let reference_immutable = &value;

    // Attempt to create a mutable reference (uncommenting this line will cause a compilation error)
    // let reference_mutable = &mut value;
    //                         ^^^^^^^^^^ mutable borrow occurs here

    println!("Value: {}", value);
    println!("Immutable Reference: {}", *reference_immutable);
}
```

You cannot have mutable references to the same data in multiple places simultaneously.

```rust
fn main() {
    let mut value = 42;

    // Create a mutable reference
    let reference1 = &mut value;

    // Attempt to create another mutable reference (uncommenting this line will cause a compilation error)
    // let reference2 = &mut value;
    //                 ^^^^^^^^^^ second mutable borrow occurs here

    // Modify the value through the first mutable reference
    *reference1 += 10;

    println!("Value: {}", value);
}
```

References have lifetimes associated with them to ensure they are valid within their scopes.

```rust
fn main() {
    let value1 = 42;
    let value2 = 24;

    let reference1: &i32;
    let reference2: &i32;

    {
        // Create a reference with a limited scope
        reference1 = &value1;
    } // 'reference1' goes out of scope here

    // Attempt to use 'reference1' after its scope has ended (uncommenting this line will cause a compilation error)
    // println!("Value from reference1: {}", *reference1);

    {
        // Create another reference with a different lifetime
        reference2 = &value2;
    } // 'reference2' goes out of scope here

    // Use 'reference2' safely
    println!("Value from reference2: {}", *reference2);
}
```

### Ownership & Functions

When you pass a reference to a function using the & symbol, you're creating an immutable borrow. It means that the function can read the data but cannot modify it.

```rust
fn print_value(value: &i32) {
    println!("Value: {}", value);
}

fn main() {
    let x = 42;
    print_value(&x); // Pass an immutable reference to 'x'
}
```

When you pass a mutable reference using the &mut symbol, you're creating a mutable borrow. It allows the function to modify the data.

```rust
fn increment(value: &mut i32) {
    *value += 1; // Dereference and modify the value
}

fn main() {
    let mut x = 42;
    increment(&mut x); // Pass a mutable reference to 'x'
    println!("Modified Value: {}", x);
}
```

You can also pass ownership of a value to a function by including the value itself as an argument. This is known as ownership transfer, and it allows the function to have full control over the data.

```rust
fn consume_value(value: String) {
    println!("Consumed: {}", value);
}

fn main() {
    let s = String::from("Hello");
    consume_value(s); // Ownership of 's' is transferred to the function
    // 's' is no longer accessible here
}

```

### Lifetime Annotations

Lifetime annotations are used to indicate how long references (borrowed data) remain valid within a given scope. They ensure that borrowed data does not outlive the data it references, preventing dangling references and memory safety issues.

```rust
fn longest<'a>(s1: &'a str, s2: &'a str) -> &'a str {
    if s1.len() > s2.len() {
        s1
    } else {
        s2
    }
}

fn main() {
    let string1 = String::from("hello");
    let result;

    {
        let string2 = String::from("world");
        result = longest(string1.as_str(), string2.as_str());
    } // 'string2' goes out of scope

    println!("The longest string is: {}", result);
} // 'string1' goes out of scope
```

### Collections

Rust provides a variety of collection types that allow you to store and manipulate data in different ways.

**`Vectors (Vec<T>)`:**
Vectors are dynamic arrays that can grow or shrink in size. They allow you to store multiple values of the same type.

```rust
let mut numbers: Vec<i32> = vec![1, 2, 3];
numbers.push(4);
numbers.pop();
```

**`Arrays ([T; N])`:**
Arrays have a fixed size determined at compile-time. They store multiple values of the same type.

```rust
let days_of_week: [&str; 7] = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"];
```

**`Strings (String, &str)`:**
Strings in Rust are UTF-8 encoded. You can use both `String` (owned string) and `&str` (string slice) types.

```rust
let greeting: String = String::from("Hello, World!");
let message: &str = "Rust is awesome!";
```

**`Hash Maps (HashMap<K, V>)`:**
Hash maps store key-value pairs and allow efficient lookups by keys.

```rust
use std::collections::HashMap;

let mut scores = HashMap::new();
scores.insert("Alice", 100);
scores.insert("Bob", 85);
```

**`Sets (HashSet<T>)`:**
Sets store unique values and provide operations for testing membership.

```rust
use std::collections::HashSet;

let mut unique_numbers = HashSet::new();
unique_numbers.insert(1);
unique_numbers.insert(2);
```

**`Slices (&[T])`:**
A slice is a data type in Rust that represents a view into a contiguous sequence of elements, such as an array, vector, or string. Slices allow you to reference a portion of the original data without copying it.

```rust
let numbers = [1, 2, 3, 4, 5];
let slice = &numbers[1..4]; // Slice from index 1 to 3
```

**`Tuples`:**
Tuples are ordered collections of values of different types. They have a fixed size.

```rust
let person: (String, i32) = ("Alice".to_string(), 30);
```

### Traits

A trait is a way to define a set of methods or behaviors that types can implement. Traits enable you to define a common interface for various types, allowing them to share common functionality without relying on inheritance.

Here's a basic overview of traits in Rust:

**`Defining a Trait`:**

To define a trait, you use the `trait` keyword followed by the trait's name and a set of method signatures. These method signatures represent the behaviors that types implementing the trait must provide.

```rust
trait Shape {
    fn area(&self) -> f64;
}
```

In this example, we've defined a `Shape` trait with a single method `area()`.

**`Implementing a Trait`:**

To make a type implement a trait, you use the `impl` keyword followed by the trait's name. Inside the `impl` block, you provide implementations for the trait's methods.

```rust
struct Circle {
    radius: f64,
}

impl Shape for Circle {
    fn area(&self) -> f64 {
        std::f64::consts::PI * self.radius * self.radius
    }
}
```

Here, we've implemented the `Shape` trait for the `Circle` struct by defining the `area()` method.

**`Using Traits`:**

Once a type implements a trait, you can use trait methods on values of that type as if they were regular methods.

```rust
fn print_area(shape: &impl Shape) {
    println!("Area: {}", shape.area());
}

let circle = Circle { radius: 2.0 };
print_area(&circle);
```

In this example, we define a function `print_area()` that takes any type implementing the `Shape` trait as an argument. We then call this function with a `Circle` instance.

**`Default Implementations`:**

Traits can provide default implementations for some or all of their methods. Types implementing the trait can choose to override these defaults or use them as-is.

```rust
trait Printable {
    fn print(&self) {
        println!("Printing from the trait");
    }
}

struct CustomType;

impl Printable for CustomType {}

fn main() {
    let custom = CustomType;
    custom.print(); // Calls the default implementation
}
```

**`Deriving Traits`:**

Rust provides built-in traits like `Debug` and `Clone` that can be derived automatically for custom types using the `#[derive]` attribute.

```rust
#[derive(Debug, Clone)]
struct Person {
    name: String,
    age: u32,
}
```

### Error Handling

**`Result Type`:**

The `Result` type is a fundamental part of error handling in Rust. It's an enum with two variants: `Ok(T)` for successful results and `Err(E)` for errors. You can use `Result` to represent functions that can return an error:

```rust
fn divide(x: i32, y: i32) -> Result<i32, String> {
    if y == 0 {
        return Err("Division by zero".to_string());
    }
    Ok(x / y)
}

fn main() {
    let result = divide(10, 2);

    match result {
        Ok(value) => println!("Result: {}", value),
        Err(error) => println!("Error: {}", error),
    }
}
```

**`panic!` Macro:**

In cases where an unrecoverable error occurs, you can use the `panic!` macro to terminate the program with an error message. Panicking is typically reserved for critical errors like out-of-bounds array access:

```rust
fn main() {
    let numbers = vec![1, 2, 3];
    let index = 5;

    if index >= numbers.len() {
        panic!("Index out of bounds");
    }

    let value = numbers[index];
    println!("Value: {}", value);
}
```

**`unwrap()` and `expect()`:**

The `unwrap()` method is a convenience method on `Result` that either returns the value if `Result` is `Ok` or panics if it's `Err`. The `expect()` method is similar but allows you to specify a custom error message when panicking:

```rust
let result: Result<i32, &str> = Err("Something went wrong");

// Using unwrap
let value = result.unwrap(); // Panics with a default error message

// Using expect
let value = result.expect("Custom error message"); // Panics with a custom error message
```

**The `?` Operator:**

The `?` operator is used within functions that return `Result`. It can be used to propagate errors up the call stack, simplifying error handling. When used, it unwraps the `Ok` variant and returns the `Err` variant early if an error occurs:

```rust
fn divide(x: i32, y: i32) -> Result<i32, String> {
    if y == 0 {
        return Err("Division by zero".to_string());
    }
    Ok(x / y)
}

fn compute(x: i32, y: i32) -> Result<i32, String> {
    let result = divide(x, y)?;
    Ok(result * 2)
}
```

### Functional Programming Features

**`First-Class Functions`:**

```rust
fn add(a: i32, b: i32) -> i32 {
    a + b
}

let func: fn(i32, i32) -> i32 = add;
let result = func(2, 3); // 5
```

**`Closures`:**

```rust
let add = |a: i32, b: i32| a + b;
let result = add(2, 3); // 5
```

**`Iterator Trait`:**

The `Iterator` trait provides methods like `map`, `filter`, and `fold`.

```rust
let numbers = vec![1, 2, 3, 4, 5];
let sum: i32 = numbers.iter().filter(|&&x| x % 2 == 0).map(|&x| x * 2).sum();
```

**`Immutable Data Structures`:**

Rust has a focus on immutability and encourages the use of immutable data structures. Libraries like `im-rs` provide persistent data structures such as vectors and hash maps.

```rust
use im::vector::Vector;

let vector = Vector::from(vec![1, 2, 3]);
let new_vector = vector.push_back(4);
```

**`Higher-Order Functions`:**

You can define and use higher-order functions, which are functions that take other functions as arguments or return functions as results.

```rust
fn apply_twice<F>(f: F, x: i32) -> i32
where
    F: Fn(i32) -> i32,
{
    f(f(x))
}
```

**`Currying`:**

```rust
fn add(x: i32) -> impl Fn(i32) -> i32 {
    move |y| x + y
}

fn main() {
    let add_five = add(5);
    let result = add_five(3); // Equivalent to add(5)(3)
    println!("Result: {}", result); // 8
}

```

### Boxing

A Box<T> is a smart pointer that provides heap allocation for data, allowing you to create values whose size is not known at compile time.

```rust
fn main() {
    // Create a boxed integer
    let boxed_value: Box<i32> = Box::new(42);

    // Access the value inside the box
    println!("Value: {}", *boxed_value);
}

```

### Next Steps

We've covered just the very basics necessary to become productive in Rust. To deepen our knowledge of this wonderful laguage here is a list of some of the most helpful learning resources on the subject:

| Resource                                                         | Description                                                            |
| ---------------------------------------------------------------- | ---------------------------------------------------------------------- | --- |
| [The Rust Programming Language](https://doc.rust-lang.org/book/) | Official Rust book with comprehensive coverage and practical examples. |
| [Rust by Example](https://doc.rust-lang.org/rust-by-example/)    | A collection of hands-on Rust examples with explanations.              |
| [Rustlings](https://github.com/rust-lang/rustlings)              | Interactive exercises to learn Rust fundamentals.                      |
| [Exercism Rust Track](https://exercism.io/tracks/rust)           | Practice Rust with coding exercises and mentorship.                    |
| [Rust documentation](https://doc.rust-lang.org/)                 | Official Rust documentation for in-depth reference.                    |
| [Rust Playground](https://play.rust-lang.org/)                   | An online code editor to experiment with Rust code.                    |
| [Awesome Rust](https://github.com/rust-unofficial/awesome-rust)  | A curated list of Rust-related resources, libraries, and tools.        |
| [Rust Reddit Community](https://www.reddit.com/r/rust/)          | Active community for discussions, questions, and news.                 |     |
| [Official Rust Forum](https://users.rust-lang.org/)              | Forum for asking questions and seeking help from the Rust community.   |
