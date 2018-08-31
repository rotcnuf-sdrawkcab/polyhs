NAME= polyhs
SRC = Parser.hs GeneralParsers.hs 	   \
      EquationParser.hs EquationTypes.hs   \
      EquationHelpers.hs EquationSolver.hs \
      main.hs
HI_F= $(SRC:.hs=.hi)
OBJ = $(SRC:.hs=.o)

$(NAME):
	ghc $(SRC) -o $(NAME)

all: $(NAME)

allc: $(NAME) clean

clean:
	rm -f $(HI_F) $(OBJ)

fclean: clean
	rm -f $(NAME)

re: fclean all
