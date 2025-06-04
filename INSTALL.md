From ubuntu, use the following commands:
(assuming a default OCaml compiler of 4.14.1 or 4.14.2)

    sudo apt update -y && sudo apt install opam -y
    opam init -y
    eval $(opam env)
    opam install . -y
