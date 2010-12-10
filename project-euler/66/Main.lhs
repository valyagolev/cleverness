\documentclass{article}

%include polycode.fmt

\begin{document}

\section{The problem}

This is a problem 66 from Project Euler, and it goes like this:

Consider quadratic Diophantine equations of the form:

\[ x^2 - Dy^2 = 1 \]

For example, when $D=13$, the minimal solution in $x$ is $ 649^2 - 13*180^2 = 1 $.

It can be assumed that there are no solutions in positive integers when $D$ is square.

By finding minimal solutions in $x$ for $D = \{2, 3, 5, 6, 7\}$, we obtain the following:

\begin{eqnarray*}
3^2 - 2*2^2 = 1\\
2^2 - 3*1^2 = 1\\
9^2 - 5*4^2 = 1\\
5^2 - 6*2^2 = 1\\
8^2 - 7*3^2 = 1
\end{eqnarray*}

Hence, by considering minimal solutions in $x$ for $D \leq 7$, the largest $x$ is obtained when $D=5$.

Find the value of $D \leq 1000$ in minimal solutions of $x$ for which the largest value of $x$ is obtained.

\section{Definitions}

Let me begin with main definitions.

The equation for a certain $D$ will be stored as an $Equation$:

> newtype Equation = Equation Int

We can test if a certain pair of $x$ and $y$ is a solution for the $Equation$:

> test :: Equation -> Int -> Int -> Bool
> test (Equation d) x y = (x^2 - d*y^2 == 1)

Let's check an example solution (from now on, every $check_*$ function should return $True$: a simple unit test):

> check_1 = test (Equation 13) 649 180

I can't make up a quick test if a certain $x$ is a minimal solution.

\section{Problem history}

This problem appears to be well-known. 

\end{document}