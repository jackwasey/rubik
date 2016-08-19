
# red white blue in place
# anticlockwise round bottom: purple blue, green white (red top), white-green (purple top)

i <- complex(imaginary = 1)
u <- exp(4 * i * pi / 3)
v <- exp(2 * i * pi / 3)
s_zero <- matrix(data = complex(16), ncol = 4L, nrow = 4L)
diag(s_zero) <- complex(1)
# each column is a fixed position with respect to the middle square/cube frame.
# 1 is equivalent to 0? It is, but it loses information about position when
# there is no twist. E.g. If all the corners were oriented correctly, then the
# matrix would be all zero, even if positions were wrong. Therefore, 1 must
# represent the correctly twisted corner.
s_init <- matrix(data = c(
  0, v, 0, 0,
  u, 0, 0, 0,
  0, 0, 0, v,
  0, 0, u, 0
), ncol = 4L)

# check assumptions
stopifnot(Mod(v) == 1)
stopifnot(Mod(u) == 1)
stopifnot(sum(Mod(s_init) != 0) == 4)

rubik <- function(s_init = s_init)
  rubik_describe_first_solution(s_init)

rubik_corner_all_solutions <- function(s_init, stop_on_first = TRUE) {
  solution_space <- expand.grid(i = 1L:8L, j = 1L:8L, k = 1L:8L,
                                l = 1L:8L, m = 1L:8L)
  row <- 1
  soln_rows <- c()
  solved_stop = FALSE
  while (!solved_stop && row != nrow(solution_space)) {
    soln <- as.integer(solution_space[row, ])
    s_test <- rubik_corner_apply(s_init, soln)
    is_solved <- rubik_solved(s_test)
    if (stop_on_first) {
      show_if_rubik_solved(s_test, solution = soln)
      solved_stop <- is_solved
    }

    if (is_solved)
      soln_rows <- append(soln_rows, row)
    row <- row + 1L
  }
  solution_space[soln_rows, ]
}

rubik_describe_first_solution <- function(s_init) {
  solutions <- rubik_corner_all_solutions(s_init = s_init)
  if (nrow(solutions) == 0L)
    stop("no solutions found")
  for (i in 1:ncol(solutions))
    describe_transform(solutions[1, i])
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

rubik_solved <- function(x, tol = 0.1) {
  isTRUE(all.equal(x, s_zero))
}

stopifnot(rubik_solved(s_zero))


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
  rubik_transform(x,
                  start_corner = corner_from_transform(trans),
                  left_handed = left_handed_from_transform(trans))
}

corner_from_transform <- function(trans) (trans + 1L) %/% 2L

left_handed_from_transform <- function(trans) ((trans + 1L) %% 2L) == 1L

describe_transform <- function(trans) {
  stopifnot(length(trans) == 1, trans >= 0, trans <= 8)
  if (trans == 0)
    message("identity")
  else
    message("start corner: ", corner_from_transform(trans),
            ifelse(left_handed_from_transform(trans) == 1, ", left_handed", ""))
}

rubik_transform <- function(x, start_corner = 1L, left_handed = FALSE) {
  stopifnot(nrow(x) == 4, ncol(x) == 4, is.matrix(x))
  stopifnot(start_corner %in% 1L:4L)
  stopifnot(is.logical(left_handed))

  # transformation matrix has fixed positions represented by rows, and col
  # showing which cube is there, (transpose of s_init arrangement)
  move_base <- matrix(data = c(
    0, 0, 0, 0,
    0, 0, 0, v,
    0, v, 0, 0,
    0, 0, v, 0
  ), ncol = 4L)

  move <- move_base

  if (start_corner == 2L) move <- move[c(4, 1, 2, 3), ]
  else if (start_corner == 2L) move <- move[c(3, 4, 1, 2), ]
  else if (start_corner == 4L) move <- move[c(2, 3, 4, 1), ]

  if (left_handed)
    move <- move[c(4, 3, 2, 1), ]

  move %*% x
}
