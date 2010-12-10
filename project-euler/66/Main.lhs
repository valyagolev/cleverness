\documentclass{article}

%include polycode.fmt
%options ghci

\usepackage{amsmath}


\long\def\ignore#1{}

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

> import Data.Maybe(isJust)

Let me begin with main definitions.

The equation for a certain $D$ will be stored as an $Equation$:

> newtype Equation = Equation Int
>   deriving (Show)

We can test if a certain pair of $x$ and $y$ is a solution for the $Equation$:

> test :: Equation -> Int -> Int -> Bool
> test (Equation d) x y = (x^2 - d*y^2 == 1)

Let's check an example solution (from now on, every $check_*$ function should return $True$: a simple unit test):

> check_1 = test (Equation 13) 649 180

I can't make up a quick test if a certain $x$ is a minimal solution.

\section{Research}

This problem appears to be well-known. The equation $x^2 - ny^2 = 1$ is named \textbf{\itshape Pell's equation}. A pair of $x = 1$ and $y = 1$ is a trivial solution; for any natural $n$ there are $x$ and $y > 0$ that satisfy the equation.

\section{Iteration}

Lagrange proved that:

\begin{equation} \label{eq:solutionexists}
\forall n \neq m^2 : \exists x > 0, y > 0 : x^2 - ny^2 = 1
\end{equation}

So we're interested in any $Equation$ $d$ there $d$ is not a perfect square:

> isqrt :: Int -> Int
> isqrt n = floor $ sqrt $ (fromIntegral n::Double)

> is_perfect_square :: Int -> Bool
> is_perfect_square n = sq * sq == n
>    where sq = isqrt n

> equations :: Int -> [Equation]
> equations max_d = map Equation $ filter (not . is_perfect_square) [1..max_d]

Since there does not seem to be a way in which $Equations$ for different $d$s are related, we can just solve every $Equation$ independently:

> minimal_solutions :: (Equation -> (Int, Int)) -> Int -> [(Equation, (Int, Int))]
> minimal_solutions solve max_d = zip eqs (map solve eqs)
>   where eqs = equations max_d

We can even parallelize it.

\section{Solving an equation}

We have to implement a $solve$ function now:

> solve :: Equation -> (Int, Int)

\ignore{

> solve = undefined

}

Let's start with a simple brute-force approach: iterate over $x \in N$ and try to calculate an $y$. If it succeeds, then we have a solution. Due to \eqref{eq:solutionexists} this process will eventually end.

\[
y^2 = \frac{x^2 - 1}{d}
\]

> divides :: Int -> Int -> Bool
> divides a b = a `mod` b == 0


> get_y :: Equation -> Int -> Maybe Int
> get_y (Equation d) x = if dy2 `divides` d && y * y == y2
>                        then Just y
>                        else Nothing
>   where dy2 = x*x - 1
>         y2 = dy2 `div` d
>         y = isqrt y2


> solve_brute :: Equation -> (Int, Int)
> solve_brute d = head solutions 
>    where solutions = map (\(x, Just y) -> (x, y)) $ filter (isJust . snd) variants
>          variants = zip [2..] $ map (get_y d) [2..]

Let's try it out on the example data set:

< minimal_solutions solve_brute 7

\begin{tt}
\eval{minimal_solutions solve_brute 7}
\end{tt}

With this naive approach calculating a minimal $x$ for $d \leq 60$ works well, but $d = 61$ takes too long to solve. Let's further investigate the problem.

\section{Chakravala method}





\end{document}