# Calyxium: **An Interpreted Multi-Paradigm Programming Language that's better than C**

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

## Why
C is such an old and broken language, with it's "strong" type system that we both know it's weak, but C has other issue, so does Rust like just being bad languages, and pretty slow imho yet people still use both of these **HORRIBLE** languages. 
- Use C if you hate yourself. 
- Use Rust if you hate others.
- Use Calyxium if you just want to get stuff done without a PhD in pain.

## Installation
Follow these steps to install and build Calyxium:

1. Install OCaml

    Download and install OCaml from the [official site](https://ocaml.org/install)
    - [Linux/macOS/BSD](https://ocaml.org/install#linux_mac_bsd)
    - [Windows](https://ocaml.org/install#windows)

2. Clone the repository
    ```bash
    git clone https://github.com/Calyxium-Lang/Calyxium.git
    cd Calyxium
    ```

3. Build the projrct
    ```bash
    dune build --profile release
    ```

4. Install the project
    ```bash
    dune install
    ```

## Getting Started
Create a new Calyxium script with the `.cx` extension. Here's a basic example:
```
println("Hello, world")
```
To run your script:
`calyxium main.cx`

For more detailed documentation, visit the [official documentation](https://calyxium.cc/docs)

## Contributing
We welcome contributions! Whether it's a bug report, feature suggestion, or code contribution:

- File issues via the [issue tracker](http://github.com/Calyxium-Lang/Calyxium/issues)
- For the repo and open a pull request

Please follow conventional commit and PR practices where possible.

## License

Calyxium is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.
