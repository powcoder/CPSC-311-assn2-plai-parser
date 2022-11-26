https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
Please fill out each TODO item in the header.

Please don't reformat the HEADER area of this file, and change nothing except the TODOs, particularly nothing before the colon ":" on each line! 

We reserve the right to do (minor) point deductions for misformatted READMEs. 

===================== HEADER ========================

Student #1, Name: TODO
Student #1, ugrad.cs.ubc.ca Login: TODO
Student #1, Student Number: TODO

Student #2, Name: TODO (or write "NONE")
Student #2, ugrad.cs.ubc.ca Login: TODO (or write "NONE")
Student #2, Student Number: TODO (or write "NONE")

Team name (for fun!): TODO

Acknowledgment that you understand and have followed the course's collaboration policy (READ IT at
http://www.ugrad.cs.ubc.ca/~cs311/current/_syllabus.php#Collaboration_Policy):

Signed: TODO (put your names here again as a signature)

===================== LOGISTICS =====================

Please fill in each of the following:

Acknowledgment of assistance (per the collab policy!): TODO

For teams, rough breakdown of work: TODO (no more than a paragraph needed!)

====================== THEORY =======================

1. Give a simple test case for your interpreter that ensures it uses eager evaluation semantics. (Your test case may not rely on the amount of time it takes the interpreter to run.)

TODO

2. As mentioned in the assignment overview, there are many ways to describe equivalent distributions. For example, (distribution '(1 1 2 2 3 3)), (distribution '(2 1 3 1 3 2)), and (distribution '(1 2 3)) are all the same (they have a 1/3 chance of a 1 or 2 or 3). We might want to be able to transform all of these to a minimal and canonical form. Minimal here means a distribution value contains as few numbers as possible. Canonical means any two equivalent distribution values transform to the same canonical distribution value. 

Describe the steps you would take to transform a distribution to a minimal, canonical form and how you could use such a transformation to check for equality.

First, sort the numbers from small to large. Then count the frequency of each number, get their greatest common denominator, each frequence divide
the GCD, finally generate the final list according to the new frequency.

Now check the equality simply using eq? to check the equality of list.


3. Just for the following question, assume that the "observe-that" doesn't exist in our language and that distributions are never empty. Suppose we modify the evaluation of distributions in the following way: When a distribution is interpreted, a single number is immediately sampled from it (with the appropriate probability of it appearing). This single value is then used in the remainder of the program's execution. This is similar to the Arithmetic Expression (AE) Language from class, but with random starting values. 

Answer these two questions: 
a) Could your program output a final value that didn't appear in the original interpreter's distribution output? 
b) How would the results of multiple runs of this new interpreter relate to the the distribution that the original interpreter would have produced?

a) No
b) They have the same distribution

======================= BONUS =======================

If you attempted any bonuses, please note it here and describe how you approached them.

TODO
