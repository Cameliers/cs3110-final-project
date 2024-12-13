# Sands of Chance

Run the following commands:

opam install cohttp-lwt-unix yojson lwt
opam install graphics
opam install lwt_ssl

To run the program: dune exec bin/main.exe

There will be seven options, enter a number into the CLI when prompted to interact with the program.
If you are running the program for the first time, you will not have a user profile and the program will take you through a tutorial.
If you are running the program with an existing user profile, your profile will be loaded and you will be taken straight to the main menu.

To run the rudimentary GUI: dune exec final_project_gui