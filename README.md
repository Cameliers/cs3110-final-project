# Sands of Chance

Authors:
- Saarang Bondalapati, sb2433
- Arman Margarian, am2738
- Andrei Codreanu, ac2923
- Abdul Raafai Asim, aa2543
- Salem Alshamsi, sa948


## Welcome to **Sands of Chance**!

This is the home of all OCaml sports betting fans. You can use this app to view upcoming soccer matches, place bets, and track your betting prowess over time!

## Getting Started

To install OCaml, follow the instructions here: https://www.cs.cornell.edu/courses/cs3110/2017fa/install.html

Navigate to the directory for the program using `cd`.

Run the following commands to install the necessary packages: 

```
opam install cohttp-lwt-unix yojson lwt
opam install graphics
opam install lwt_ssl
```

To run the program:
```
dune exec bin/main.exe
```

There will be seven options, enter a number into the CLI when prompted to interact with the program.
If you are running the program for the first time, you will not have a user profile and the program will take you through a tutorial.
If you are running the program with an existing user profile, your profile will be loaded and you will be taken straight to the main menu.


To run the rudimentary GUI:
```
dune exec final_project_gui
```

We have a youtube video demo for our program available:
https://www.youtube.com/watch?v=vl0h7JZycdw