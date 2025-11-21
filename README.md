# Calyxium: **A multi-paradigm, memory-safe, monomorphic, strongly typed language with ad-hoc polymorphism.**

<!-- TOC -->

<p align="left"> 
    <a href="#why">Why</a> |
    <a href="#installation">Installation</a> | 
    <a href="#getting-started">Getting Started</a> | 
    <a href="#usage">Usage</a> | 
    <a href="#contributing">Contributing</a> | 
    <a href="#license">License</a>
</p>

<!-- TOC -->

![Build Status](https://github.com/calyxium-lang/calyxium/actions/workflows/ci.yml/badge.svg)

## Why
Calyxium is designed with safety and clarity first:
- **Generational Mark/Sweep Garbage Collector**: Automatic memory management ensures memory safety without manual intervention.
- **Type Safety with Explicitnes**: Strong static typing with support for both polymorphism and monomorphism. You can rely on inference or declare explicit types for precision
    ```
    let x = 10 in
    let y: int = 10 in
    ```
- **Immutable by Default**: Values cannot be mutated, reducing bugs from shared state. Mutation is only possible through explicit **refs**, making mutability intentional and controlled.
- **Ad-Hoc Polymorphism**: Flexible polymorphism without sacrificing type safety, allowing expressive yet predictable code.  

Calyxium exists to provide a modern, safe, and expressive programming language that balances functional purity with pragmatic expressiveness.

## Installation
Follow these steps to install and build Calyxium:

1. Install OCaml

    Download and install OCaml from the [official site](https://ocaml.org/install)
    - [Linux/macOS/BSD](https://ocaml.org/install#linux_mac_bsd)
    - [Windows](https://ocaml.org/install#windows)

2. Clone the repository
    ```bash
    git clone https://github.com/calyxium-lang/calyxium.git
    cd calyxium
    ```
    
3. Install the dependency
    ```bash
    opam install menhir
    ```

4. Build the project
    ```bash
    dune build --profile release
    ```

5. Install the project
    ```bash
    dune install
    ```

## Getting Started

For detailed documentation, visit the [official documentation](https://calyxium-lang.github.io/docs)

## Contributing
We welcome contributions! Whether it's a bug report, feature suggestion, or code contribution:

- File issues via the [issue tracker](http://github.com/calyxium-lang/calyxium/issues)
- For the repo and open a pull request

Please follow [conventional commit and PR practices where possible](CONTRIBUTING.md).

## License

Calyxium is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.
