
(* Coq provides a string library, which allows you 
to define and manipulate strings in a type-safe manner. 
The library is based on the standard ASCII character 
set and provides various functions for string manipulation,
such as concatenation, substring extraction, and character encoding.
*)
 
Require Import String. 
Require Import Ascii. 
Require Import List.  
Import Notations ListNotations.
 
Definition hello_world : string := "Hello, world!"%string.

Definition substring : string := substring 0 5 hello_world.

Definition length : nat := String.length hello_world. 
 
Check String.concat. 

Check String.concat "aa"%string .

(* Concatena gli elementi nella lista di stringhe, 
separando con lo spazio " " *)
Definition concat : string := 
   String.concat " "%string [hello_world; substring].

(* Here, we define a string variable hello_world and initialize 
it with the value "Hello, world!". We then define another string 
variable substring that contains the substring "Hello" extracted 
from hello_world. We also define a natural number variable length 
that contains the length of hello_world, which is 13. Finally, we
define a string variable concat that contains the concatenation 
of hello_world and substring.

The %string notation is used to indicate that the string is of 
type string in Coq. This is necessary because Coq has a built-in 
list type, which can also be used to represent strings.

The String.length function is used to compute the length of a string. 
The String.concat function is used to concatenate a list of strings 
into a single string, with an optional separator string specified as 
the first argument (in this case, we use an empty separator).

You can also use various other functions provided by the string library, 
such as String.get to retrieve a character at a specific position, 
String.substring to extract a substring, 
and String.rev to reverse a string. 
 
The library also provides functions for encoding and decoding strings 
from various character sets, such as UTF-8, ASCII, and ISO-8859-1.

Here is an example that demonstrates some of these functions:
*)
 
Definition str : string := "abcde"%string.
Definition char : option ascii := String.get 2 str.
 
(* Definition rev : string := String.rev str *)
(* Definition encode : string := String.utf8 "こんにちは". *)
(* Definition decode : string := String.utf8_decode encode. *)

(* In this example, we define a string variable str and 
initialize it with the value "abcde". We then use the 
String.get function to retrieve the character at position 
2 (which is 'c'), and store it in an ascii variable char. 
also define a string variable rev that contains the reversed 
string "edcba". Finally, we define a string variable encode 
that contains the UTF-8 encoded string for the Japanese 
greeting "こんにちは", and another string variable decode that 
contains the decoded version of encode.
*)
 