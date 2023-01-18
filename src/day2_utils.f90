module day2_utils
   implicit none

   private
   public :: calc_round, fix_round
contains
   function calc_round(opponent, move) result(score)
      !! Calculate the score for a game of rps

      character, intent(in) :: opponent
         !! The move for the opponent
      character, intent(in) :: move
         !! The move to choose

      integer :: score
         !! The score to return

      character :: op_rock
         !! Character for opponent choosing rock
      character :: op_paper
         !! Character for opponent choosing paper
      character :: op_scissors
         !! Character for opponent choosing scissors

      character :: rock
         !! Character for choosing rock
      character :: paper
         !! Character for choosing paper
      character :: scissors
         !! Character for choosing scissors

      integer :: rock_score
         !! Score for choosing rock
      integer :: paper_score
         !! Score for choosing paper
      integer :: scissors_score
         !! Score for choosing scissors

      integer :: round_score
         !! Score for a round

      op_rock = 'A'
      op_paper = 'B'
      op_scissors = 'C'
      rock = 'X'
      paper = 'Y'
      scissors = 'Z'

      rock_score = 1
      paper_score = 2
      scissors_score = 3

      round_score = 6

      score = 0
      if (move == rock) score = rock_score
      if (move == paper) score = paper_score
      if (move == scissors) score = scissors_score

      ! print *, "Move score: ", score
      if ((move == rock .and. opponent == op_paper) &
          .or. (move == paper .and. opponent == op_scissors) &
          .or. (move == scissors .and. opponent == op_rock)) then
         score = score + 0
      else if ((move == rock .and. opponent == op_rock) &
               .or. (move == paper .and. opponent == op_paper) &
               .or. (move == scissors .and. opponent == op_scissors)) then
         score = score + round_score/2
      else if ((move == rock .and. opponent == op_scissors) &
               .or. (move == paper .and. opponent == op_rock) &
               .or. (move == scissors .and. opponent == op_paper)) then
         score = score + round_score
      end if

      ! print *, "Total score: ", score
   end function calc_round

   function fix_round(opponent, result) result(score)
      !! Return the score for the given result

      character, intent(in) :: opponent
         !! The opponents move
      character, intent(in) :: result
         !! The result to aim for

      integer :: score
         !! The score to return

      character :: op_rock
         !! Character for opponent choosing rock
      character :: op_paper
         !! Character for opponent choosing paper
      character :: op_scissors
         !! Character for opponent choosing scissors

      character :: lose
         !! Character for choosing to lose
      character :: draw
         !! Character for choosing to draw
      character :: win
         !! Character for choosing to win

      integer :: rock_score
         !! Score for choosing rock
      integer :: paper_score
         !! Score for choosing paper
      integer :: scissors_score
         !! Score for choosing scissors

      integer :: round_score
         !! Score for a round

      op_rock = 'A'
      op_paper = 'B'
      op_scissors = 'C'
      lose = 'X'
      draw = 'Y'
      win = 'Z'

      rock_score = 1
      paper_score = 2
      scissors_score = 3

      round_score = 6

      if (result == win) score = round_score
      if (result == draw) score = round_score/2
      if (result == lose) score = 0

      if ((result == win .and. opponent == op_scissors) &
          .or. (result == draw .and. opponent == op_rock) &
          .or. (result == lose .and. opponent == op_paper)) then
         score = score + rock_score
      elseif ((result == win .and. opponent == op_rock) &
              .or. (result == draw .and. opponent == op_paper) &
              .or. (result == lose .and. opponent == op_scissors)) then
         score = score + paper_score
      elseif ((result == win .and. opponent == op_paper) &
              .or. (result == draw .and. opponent == op_scissors) &
              .or. (result == lose .and. opponent == op_rock)) then
         score = score + scissors_score
      end if
   end function fix_round
end module day2_utils
