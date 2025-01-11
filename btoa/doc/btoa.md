$$
\newcommand{\bx}{\boldsymbol{x}}
$$

## B-splines

B-splines are defined recursively from

$$
N_j^0(x) = \begin{cases}
1, & x_j \le x < x_{j+1}, \\
0, & \text{otherwise}
\end{cases}
$$
and
$$
N_j^p(x) = w_j^p(x) N_j^{p-1}(x) + (1-w_{j+1}^p(x)) N_{j+1}^{p-1}(x),
$$

where

$$
w_j^p(x) = \frac{x - x_j}{x_{j+1}-x_j}.
$$


## Tensor product B-splines

We use tensor product B-splines to approximate scalar functions in 3D
(see, e.g. https://arxiv.org/pdf/1609.03053):

$$
\phi_h(\bx) = \sum_{i,j,k} \varphi_{ijk} N_i^p(x) N_j^p(y) N_k^p(z)
$$
