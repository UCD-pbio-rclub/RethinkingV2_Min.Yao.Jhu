---
title: "test"
author: "Min-Yao"
date: "2020/3/6"
output: 
  html_document: 
    keep_md: yes
---

$$
y _i ∼ Normal(µ_i, σ) \\
µi = α _{group[i]} + βx_i \\
α_{group} ∼ Normal(α, σ _α ) \\
α ∼ Normal(0, 10) \\
β ∼ Normal(0, 1) \\
σ ∼ HalfCauchy(0, 2) \\
σ _α ∼ HalfCauchy(0, 2) \\
$$

$$
y _i ∼ Normal(µ_i, σ) \\
µi = α _{group[i]} + β _{group[i]}*x_i \\
\begin{bmatrix}α _{group}\\
β _{group}
\end{bmatrix} \sim MVNormal( \begin{bmatrix}α\\
β
\end{bmatrix} , S ) \\
S = \begin{pmatrix}σ _α & 0\\
0 & σ _α
\end{pmatrix} * R * \begin{pmatrix}σ _α & 0\\
0 & σ _α
\end{pmatrix} \\
α ∼ Normal(0, 10) \\
β ∼ Normal(0, 1) \\
σ ∼ HalfCauchy(0, 2) \\
σ _α ∼ HalfCauchy(0, 2) \\
σ _β ∼ HalfCauchy(0, 2) \\
R \sim LKJcorr(2) \\
$$
