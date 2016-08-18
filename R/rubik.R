

i <- complex(imaginary = 1)
t <- exp(2 * i * pi / 3)


rubik_corner_solve <- function() {
  s_zero <- matrix(data = complex(16L), ncol = 4L, nrow = 4L)

  s_init <- matrix(data = c(
    0, 0, 0, 0,
    0, 0, t, 0,
    0, 0, 0, t,
    0, t, 0, 0
  ), ncol = 4L)

  # TODO just power through start of solution space



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
