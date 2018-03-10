(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* Taylor Koth
* taylorkoth@gmail.com
*
***************************************************************)

(* Define your data type and functions here *)

(*Function used in question 1*)
fun f [] = [] (*Define the function. The function will take in an int list and output a different int list*)
| f (x::xs) = (x + 1) :: (f xs); (*Bind x to the head of the int list, and xs to the tail. Then, add one to the head and recursively call the function on the tail.*)


(*Construct a datatype to represent sets, called set, with two different types: Set and Empty.
 * Where Set is of type 'element * 'element set, meaning that it holds some polymorphic type 'element
 * and a sets of 'element.*)
datatype 'element set =
    Empty
  | Set of 'element * 'element set;

(* This function will determine if an element 'e' is part of the set, set.
 * It will return true if 'e' is a memeber of the set, set.
 * Otherwise, it will return false.*)
fun isMember e Empty = false
  | isMember e (Set (x,xs)) =    (* set x to the head, xs to tail*)
      e = x                      (* check the head against the desired element*)
      orelse isMember e xs;

(* This function is going to convert a list into a set ensuring that the properties of a well formed set
 * This means no duplicates.*)
fun list2Set [] = Empty
  | list2Set (hd::tl) = Set(hd, list2Set(tl)); (* Add the first element of the list (the head(hd)) to the set, and then make a tail recursion call to continue through the set*)

(* This function is going to join two sets in a union*)
fun union Empty Empty = Empty    (* I want to cover general cases that may happen. If the two sets are empty, return empty*)
  | union set Empty = set        (* These two lines are for the cases in which one set in empty *)
  | union Empty set = set        (* These two lines are for the cases in which one set in empty *)
  | union (Set (hd, tl)) set2 =  (* Set the head and tail of the first set to be used in the union operation*)
      if isMember hd set2 then union tl set2   (* Check to see if the head is in the second set. If it is, we move on*)
      else Set(hd, (union tl set2));           (* Add the head to the new set then recurse.*)


(* This function is going to take the intersections of sets set1 and set2 and return the set representing this intersection*)
fun intersect Empty Empty = Empty    (*Again, I want to cover those general cases that I covered in the Union function.*)
  | intersect set Empty = set
  | intersect Empty set = set
  | intersect (Set (hd, tl)) set2 =   (*Set the head and tail of the first set to be used in the intersect operation*)
      if isMember hd set2 then Set (hd, (intersect tl set2))  (* Check to see if the head is in the second set, if it is, add it as the head*)
      else intersect tl set2;                                (* If it is not, ignore and move on.*)


(* Simple function to stringify the contents of a Set of characters *)
fun stringifyCharSet Empty = ""
  | stringifyCharSet (Set(y, ys)) = Char.toString(y) ^ " " ^ stringifyCharSet(ys);

(* Simple function to stringify the contents of a Set of ints *)
fun stringifyIntSet Empty = ""
  | stringifyIntSet (Set(w, ws)) = Int.toString(w) ^ " " ^ stringifyIntSet(ws);

(* Simple function to stringify the contents of a Set of strings *)
fun stringifyStringSet Empty = ""
  | stringifyStringSet (Set(z, zs)) = z ^ " " ^ stringifyStringSet(zs);

(* Simple function that prints a set of integers *)
fun print_int x = print ("{ " ^ stringifyIntSet(x) ^ "}\n");

(* Simple function that prints a set of strings *)
fun print_str x = print ("{ " ^ stringifyStringSet(x) ^ "}\n");

(* Simple function that prints a set of characters *)
fun print_chr x = print ("{ " ^ stringifyCharSet(x) ^ "}\n");

list2Set [1, 3, 2];
list2Set [#"a", #"b", #"c"];
list2Set [];
list2Set [6, 2, 2];
list2Set ["x", "y", "z", "x"];

(* Question 1 *)
print "\nQuestion 1:\n";
f [3, 1, 4, 1, 5, 9];

(* Question 5 *)
val quest5 = isMember "one" (list2Set ["1", "2", "3", "4"]);
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");

(* Question 7 *)
val quest7 = list2Set ["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"];
print "\nQuestion 7: ";
print_str quest7;
print "\n";

(* Question 9 *)
print "\nQuestion 9: ";
print_str (union (list2Set ["green", "eggs", "and"]) (list2Set ["ham"]));

(* Question 10 *)
print "\nQuestion 10: ";
print_str (intersect (list2Set ["stewed", "tomatoes", "and", "macaroni"]) (list2Set ["macaroni", "and", "cheese"]));
