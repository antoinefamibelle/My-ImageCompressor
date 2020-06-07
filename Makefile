##
## EPITECH PROJECT, 2017
## Makefile
## File description:
## repository Makefile
## haskell compilation makefile
##

NAME	=	imageCompressor

F1	=	imageCompressorProject/$(NAME)

LIBSHAREDNAME	=	$(NAME).so

COMPLIB	=	ar rc

all:	$(LIBSHAREDNAME)

$(LIBSHAREDNAME):
	(cd $(F1); make all)

clean:
	(cd $(F1); make clean)

fclean:
	(cd $(F1); make fclean)

re:	fclean all

.PHONY:	clean fclean re
