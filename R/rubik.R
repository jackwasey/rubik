

i <- complex(imaginary = 1)
t <- exp(2 * i * pi / 3)
s_zero <- matrix(data = complex(16L), ncol = 4L, nrow = 4L)
s_init <- matrix(data = c(
  0, 0, 0, 0,
  0, 0, t, 0,
  0, 0, 0, t,
  0, t, 0, 0
), ncol = 4L)

rubik_corner_solve <- function(progress = FALSE) {
  solution_space <- expand.grid(i = 0L:8L, j = 0L:8L, k = 0L:8L,
                                l = 0L:8L, m = 0L:8L, n = 0L:8L)
  is_solved <- FALSE
  row <- 1
  while (!is_solved && row <= nrow(solution_space)) {
    soln <- as.integer(solution_space[row, ])
    if (progress) message("working on ", paste(soln, collapse = ", "))
    s_test <- rubik_corner_apply(s_init, soln)
    is_solved <- show_if_rubik_solved(s_test, solution = soln)
    row <- row + 1L
  }
}

rubik_corner_apply <- function(x, transforms) {
  stopifnot(all(transforms >= 0 & transforms <= 8),
            all(is.integer(transforms)))
  stopifnot(is.matrix(x), is.complex(x))
  for (m in 1:length(transforms)) {
    x <- rubik_transform_by_num(x, transforms[m])
  }
  x
}

rubik_solved <- function(x, tol = 0.1)
  all(Mod(x) < tol)

show_if_rubik_solved <- function(x, solution) {
  solved <- rubik_solved(x)
  if (solved)
    message("transforms: ", paste(solution, collapse = ", "))
  solved
}

rubik_transform_by_num <- function(x, trans = 0L) {
  stopifnot(trans >= 0, trans <= 8, length(trans) == 1L, is.integer(trans))
  stopifnot(is.matrix(x), is.complex(x))
  if (trans == 0L) return(x)
  rubik_transform(x, start_corner = trans %/% 2, left_handed = trans %% 2)
}

rubik_transform <- function(x, start_corner = 1L, left_handed = FALSE) {
  stopifnot(nrow(x) == 4, ncol(x) == 4, is.matrix(x))

  t_base <- matrix(data = c(
    0, 0, 0, 0,
    0, 0, 0, t,
    0, t, 0, 0,
    0, 0, t, 0
  ), ncol = 4L)

  t <- t_base

  if (start_corner == 2L) t <- t[c(4, 1, 2, 3), ]
  else if (start_corner == 2L) t <- t[c(3, 4, 1, 2), ]
  else if (start_corner == 4L) t <- t[c(2, 3, 4, 1), ]

  if (left_handed)
    t <- t[c(4, 3, 2, 1), ]

  t %*% x
}
