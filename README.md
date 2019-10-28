# lambda - an educational lambda calculus language

`lambda` is a programming language purely based on the (untyped) lambda calculus and its beta-reduction. 

## Installation

## Running


## Terms

The basic syntax for creating term is,
  * any alphanumeric string is a variable. In addition the `'`  and the `_` symbol are allowed. (e.g. `a`, `x'`, `8`, `ab_8`) **The `_` symbol on its own is no Variable**
  * a `\` followed by one or more variables, then a `.` and another term is an abstraction. (e.g. `\x.x`, `\x y.x`)
  * a ` `(Space) denotes the abstraction. (e.g. `p q`, `(\x.x.) q`), `p q s`)

Parentheses are optional. If they are not present, the binding of operators follow the usual conventions for lambda calculus.

## Other Commands

To form a program you can use the following commands

+------------------------------+----------------------------------------------------------------+
|  **Command**                 |  **Description**                                               |
|  `import` File               |  Imports a file and executes it                                |
|  `let ` Variable ` = ` Term  |  Defines a Variable to be expanded to a Term in later commands |
|  `print` Text                |  Prints Text without a newline                                 |
|  `printLN` Text              |  Prints Text with a newline                                    |
|  `printT` Term               |  Prints the definition of a Term                               |
|  `printNF` Term              |  Calculates the normal form for a Term and prints it           |
|  `printNFMax` Int Term       |  As `printNF` but only computes a given number of steps        |
|  `traceNF` Term              |  Calculates the normal form for a Term and prints every step   |
|  `traceNFMax` Int Term       |  As `traceNF` but only computes a given number of steps        |
|  `step` Variable             |  Applies beta reduction on the term stored in the Variable     |
|  `set` Option Value          |  Sets an Option to a Value (see below)                         |
|  `get` Option                |  Prints the value for an Option                                |
|  `addRev` Variable           |  Notes, that terms corresponding to the Variable will be printed as the Variable, not as the Term  |
|  `delRev` Variable           |  Removes the Variable from the above list                      |
|  `showRev`                   |  Shows all variables, that will not be shown as lambdaterms    |
+------------------------------+----------------------------------------------------------------+

## Configuration

The following Options exist and can be set with `set`

+--------------+-----------------------+---------------------+-------------------------------------+
|  **Option**  |  **Possible Values**  |  **Default Value**  |  **Descrition**                     |
|  `utf8`      |  `True`, `False`      |  `False`            |  Show a real lambda, beta and arrow |
|  `explicit`  |  `True`, `False`      |  `False`            |  Shows every parentheses            |
|  `pnat`      |  `True`, `False`      |  `True`             |  Interpret natural numbers as church numerals  |
|  `rnat`      |  `True`, `False`      |  `False`            |  If a Term looks like a church numeral, show the number  |
|  `ps1`       |  Any String           |  `LAMBDA`           |  Prompt to show in REPL mode        |
|  `steps`     |  Any natural number   |  `100`              |  Steps to calculate in REPL mode    |
+--------------+-----------------------+---------------------+-------------------------------------+
