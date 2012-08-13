%{
  #include<stdio.h>
  #include<string.h>
  #include<stdlib.h>
  int yylex(void) ;
  void yyerror(char *) ;
  char integer[3] = "%d" ;
  char doublenum[4] = "%lf" ;
  char longinteger[4] = "%ld" ;
  char longlonginteger[5] = "%lld" ;
  char floatnum[3] = "%f" ;
  char character[3] = "%c" ;
  int min ;
  int max ;


%}

%union {char str[50] ; long long int num ; float fnum ;}
%token <num> INTEGER
%token <str> STRING
%token <str> INT
%token <str> LONGINT
%token <str> LONGLONGINT
%token <str> PRINT 
%token <str> VARIABLE
%token <str> IF
%token <str> CONDITION
%token <str> ELSE
%token <str> FOR
%token <str> NUMBER
%token <str> INPUT
%token <str> START
%token <str> END
%token <str> MAIN
%token <str> DEF
%token <str> CALL
%token <str> RETURN
%token <str> DO
%token <str> WHILE
%token <str> ENDIF
%token <str> ENDELSE
%token <str> ENDDO
%token <str> ENDFOR
%token <str> MIN
%token <str> ARRAY
%token <str> MAX
%token <str> FACTORIAL
%token <str> MSTRING
%token <str> FIRST
%token <str> LAST
%token <str> SORT
%token <str> CHARACTER
%token <str> DOUBLENUM
%token <str> FLOATNUM
%token <str> CHARN
%token <str> DOUBLEN
%token <str> FLOATN
%token <str> CHAR
%token <str> DOUBLE
%token <str> FLOAT
%token <str> FSTRING
%token <str> STRUCT
%token <str> STRUCTSTRING
%token <str> POINTER
%token <str> POINTS
%token <str> EXPR
%token <str> FVARIABLE
%token <str> CVARIABLE
%token <str> IVARIABLE
%token <str> LIVARIABLE
%token <str> LLIVARIABLE
%token <str> DVARIABLE
%token <str> NSTRING
%token <str> YSTRING
%token <str> VOID
%token <num> FINTEGER
%token <num> BREAK
%token <num> CONTINUE
%token <num> EXIT
%token <num> IN
%token <str> PTRSTRING
%token <str> MALLOC
%token <str> MEMORY
%token <str> LONGINTEGER
%token <str> LONGLONGINTEGER
%token <str> PRSTRING


%%



program   :     program main
                { ;}
          |     program function
                { ;}
          |     function
                { ;}
          |
          ;

function  :     DEF INT STRING
                {
                    printf("%s %s(" , $2 , $3) ;
                }
                parameter  START
                {
                    printf(") {\n") ;
                }
                prog END
                {
                    printf("}\n\n\n") ;
                }
          |     DEF LONGINT STRING
                {
                    printf("%s %s(" , $2 , $3) ;
                }
                parameter  START
                {
                    printf(") {\n") ;
                }
                prog END
                {
                    printf("}\n\n\n") ;
                }
          |     DEF LONGLONGINT STRING
                {
                    printf("%s %s(" , $2 , $3) ;
                }
                parameter START
                {
                    printf(") {\n") ;
                }
                prog END
                {
                    printf("}\n\n\n") ;
                }
          |     DEF CHAR STRING
                {
                    printf("%s %s(" , $2 , $3) ;
                }
                parameter START
                {
                    printf(") {\n") ;
                }
                prog END
                {
                    printf("}\n\n\n") ;
                }
          |     DEF FLOAT STRING
                {
                    printf("%s %s(" , $2 , $3) ;
                }
                parameter START
                {
                    printf(") {\n") ;
                }
                prog END
                {
                    printf("}\n\n\n") ;
                }
          |     DEF DOUBLE STRING
                {
                    printf("%s %s(" , $2 , $3) ;
                }
                parameter START
                {
                    printf(") {\n") ;
                }
                prog END
                {
                    printf("}\n\n\n") ;
                }
          |     DEF VOID STRING
                {
                    printf("%s %s(" , $2 , $3) ;
                }
                parameter START
                {
                    printf(") {\n") ;
                }
                prog END
                {
                    printf("}\n\n\n") ;
                }
          |     STRUCT STRING
                {
                    printf("struct %s {\n" , $2) ;
                }
                prog END
                {
                    printf("}\n\n") ;
                }
          ;

parameter :     INT STRING
                {
                    printf("%s %s" , $1 , $2) ;
                }
          |     parameter INT STRING
                {
                    printf(",%s %s" , $2 , $3) ;
                }
          |     LONGINT STRING
                {
                    printf("%s %s" , $1 , $2) ;
                }
          |     parameter LONGINT STRING
                {
                    printf(",%s %s" , $2 , $3) ;
                }
          |     LONGLONGINT STRING
                {
                    printf("%s %s" , $1 , $2) ;
                }
          |     parameter LONGLONGINT STRING
                {
                    printf(",%s %s" , $2 , $3) ;
                }
          |     CHAR STRING
                {
                    printf("%s %s" , $1 , $2) ;
                }
          |     parameter CHAR STRING
                {
                    printf(",%s %s" , $2 , $3) ;
                }
          |     FLOAT STRING
                {
                    printf("%s %s" , $1 , $2) ;
                }
          |     parameter FLOAT STRING
                {
                    printf(",%s %s" , $2 , $3) ;
                }
          |     DOUBLE STRING
                {
                    printf("%s %s" , $1 , $2) ;
                }
          |     parameter DOUBLE STRING
                {
                    printf(",%s %s" , $2 , $3) ;
                }
          | 
          ;



main      :     START MAIN
                {
                    printf("main(){\n") ;
                }
                prog END MAIN
                {
                    printf("}\n\n") ;
                }
          ;

prog      :     prog input              
                {;}
          |     input
                {;}
          ;



input     :     INT STRING '=' INTEGER
                {
                    printf("%s %s = %lld;\n" , $1 , $2 , $4) ;
                }
          |     LONGINT STRING '=' INTEGER
                {
                    printf("%s %s = %lld;\n" , $1 , $2 , $4) ;
                }
          |     LONGLONGINT STRING '=' INTEGER
                {
                    printf("%s %s = %lld;\n" , $1 , $2 , $4) ;
                }
          |     BREAK
                {
                    printf("break ;\n") ;
                }
          |     CONTINUE
                {
                    printf("continue ;\n") ;
                }
          |     EXIT
                {
                    printf("exit ;\n") ;
                }
          |     INT STRING
                {
                    printf("%s %s;\n" , $1  , $2) ;
                }
          |     LONGINT STRING
                {
                    printf("%s %s;\n" , $1  , $2) ;
                }
          |     LONGLONGINT STRING
                {
                    printf("%s %s;\n" , $1  , $2) ;
                }
          |     STRING '=' INTEGER
                {
                    printf("%s = %lld;\n" , $1  , $3) ;
                }
          |     FLOAT STRING '=' FLOATN
                {
                    printf("%s %s = %s;\n" , $1 , $2 , $4) ;
                }
          |     FLOAT STRING
                {
                    printf("%s %s;\n" , $1  , $2) ;
                }
          |     STRING '=' FLOATN
                {
                    printf("%s = %s;\n" , $1  , $3) ;
                }
          |     DOUBLE STRING '=' DOUBLEN
                {
                    printf("%s %s = %s;\n" , $1 , $2 , $4) ;
                }
          |     DOUBLE STRING
                {
                    printf("%s %s;\n" , $1  , $2) ;
                }
          |     STRING '=' DOUBLEN
                {
                    printf("%s = %s;\n" , $1  , $3) ;
                }
          |     CHAR STRING '=' CHARN
                {
                    printf("%s %s = %s;\n" , $1 , $2 , $4) ;
                }
          |     CHAR STRING
                {
                    printf("%s %s;\n" , $1  , $2) ;
                }
          |     STRING '=' CHARN
                {
                    printf("%s = %s;\n" , $1  , $3) ;
                }
          |     INPUT IVARIABLE
                {
                    printf("scanf(\"%s\" , &%s);\n" , integer , $2) ;
                }
          |     INPUT LIVARIABLE
                {
                    printf("scanf(\"%s\" , &%s);\n" , longinteger , $2) ;
                }
          |     INPUT LLIVARIABLE
                {
                    printf("scanf(\"%s\" , &%s);\n" , longlonginteger , $2) ;
                }
          |     INPUT CVARIABLE
                {
                    printf("scanf(\"%s\" , &%s);\n" , character , $2) ;
                }
          |     INPUT FVARIABLE
                {
                    printf("scanf(\"%s\" , &%s);\n" , floatnum , $2) ;
                }
          |     INPUT IVARIABLE '[' INTEGER ']'
                {
                    printf("scanf(\"%s\" , &%s[%lld]);\n" , integer , $2 , $4) ;
                }
          |     INPUT LIVARIABLE '[' INTEGER ']'
                {
                    printf("scanf(\"%s\" , &%s[%lld]);\n" , longinteger , $2 , $4) ;
                }
          |     INPUT LLIVARIABLE '[' INTEGER ']'
                {
                    printf("scanf(\"%s\" , &%s[%lld]);\n" , longlonginteger , $2 , $4) ;
                }
          |     INPUT CVARIABLE '[' INTEGER ']'
                {
                    printf("scanf(\"%s\" , &%s[%lld]);\n" , character , $2 , $4) ;
                }
          |     INPUT FVARIABLE '[' INTEGER ']'
                {
                    printf("scanf(\"%s\" , &%s[%lld]);\n" , floatnum , $2 , $4) ;
                }
          |     INPUT INT NSTRING
                {
                    printf("int %s ;\n" , $3) ;
                    printf("scanf(\"%s\" , &%s);\n" , integer , $3) ;
                }
          |     INPUT INT YSTRING
                {
                    printf("scanf(\"%s\" , &%s);\n" , integer , $3) ;
                }
          |     INPUT LONGINT NSTRING
                {
                    printf("long int %s ;\n" , $3) ;
                    printf("scanf(\"%s\" , &%s);\n" , longinteger , $3) ;
                }
          |     INPUT LONGINT YSTRING
                {
                    printf("scanf(\"%s\" , &%s);\n" , longinteger , $3) ;
                }
          |     INPUT LONGLONGINT NSTRING
                {
                    printf("long long int %s ;\n" , $3) ;
                    printf("scanf(\"%s\" , &%s);\n" , longlonginteger , $3) ;
                }
          |     INPUT LONGLONGINT YSTRING
                {
                    printf("scanf(\"%s\" , &%s);\n" , longlonginteger , $3) ;
                }
          |     INPUT CHAR NSTRING
                {
                    printf("char %s ;\n" , $3) ;
                    printf("scanf(\"%s\" , &%s);\n" , character , $3) ;
                }
          |     INPUT CHAR YSTRING
                {
                    printf("scanf(\"%s\" , &%s);\n" , character , $3) ;
                }
          |     INPUT FLOAT NSTRING
                {
                    printf("float %s ;\n" , $3) ;
                    printf("scanf(\"%s\" , &%s);\n" , floatnum , $3) ;
                }
          |     INPUT FLOAT YSTRING
                {
                    printf("scanf(\"%s\" , &%s);\n" , floatnum , $3) ;
                }
          |     INPUT DOUBLE NSTRING
                {
                    printf("double %s ;\n" , $3) ;
                    printf("scanf(\"%s\" , &%s);\n" , doublenum , $3) ;
                }
          |     INPUT DOUBLE YSTRING
                {
                    printf("scanf(\"%s\" , &%s);\n" , doublenum , $3) ;
                }
          |     INPUT INT NSTRING '[' INTEGER ']'
                {
                    printf("int %s[%lld] ;\n" , $3 ,$5) ;
                    printf("for(loopvariable = 0 ; loopvariable < %lld ; loopvariable++)\n" , $5) ;
                    printf("scanf(\"%s\" , &%s[loopvariable]);\n" , integer , $3) ;
                }
          |     INPUT INT YSTRING '[' INTEGER ']'
                {
                    printf("for(loopvariable = 0 ; loopvariable < %lld ; loopvariable++)\n" , $5) ;
                    printf("scanf(\"%s\" , &%s[loopvariable]);\n" , integer , $3) ;
                }
          |     INPUT LONGINT NSTRING '[' INTEGER ']'
                {
                    printf("int %s[%lld] ;\n" , $3 ,$5) ;
                    printf("for(loopvariable = 0 ; loopvariable < %lld ; loopvariable++)\n" , $5) ;
                    printf("scanf(\"%s\" , &%s[loopvariable]);\n" , longinteger , $3) ;
                }
          |     INPUT LONGINT YSTRING '[' INTEGER ']'
                {
                    printf("for(loopvariable = 0 ; loopvariable < %lld ; loopvariable++)\n" , $5) ;
                    printf("scanf(\"%s\" , &%s[loopvariable]);\n" , longinteger , $3) ;
                }
          |     INPUT LONGLONGINT NSTRING '[' INTEGER ']'
                {
                    printf("int %s[%lld] ;\n" , $3 ,$5) ;
                    printf("for(loopvariable = 0 ; loopvariable < %lld ; loopvariable++)\n" , $5) ;
                    printf("scanf(\"%s\" , &%s[loopvariable]);\n" , longlonginteger , $3) ;
                }
          |     INPUT LONGLONGINT YSTRING '[' INTEGER ']'
                {
                    printf("for(loopvariable = 0 ; loopvariable < %lld ; loopvariable++)\n" , $5) ;
                    printf("scanf(\"%s\" , &%s[loopvariable]);\n" , longlonginteger , $3) ;
                }
          |     INPUT CHAR NSTRING '[' INTEGER ']'
                {
                    printf("char %s[%lld] ;\n" , $3 ,$5) ;
                    printf("for(loopvariable = 0 ; loopvariable < %lld ; loopvariable++)\n" , $5) ;
                    printf("scanf(\"%s\" , &%s[loopvariable]);\n" , character , $3) ;
                }
          |     INPUT CHAR YSTRING '[' INTEGER ']'
                {
                    printf("for(loopvariable = 0 ; loopvariable < %lld ; loopvariable++)\n" , $5) ;
                    printf("scanf(\"%s\" , &%s[loopvariable]);\n" , character , $3) ;
                }
          |     INPUT FLOAT NSTRING '[' INTEGER ']'
                {
                    printf("float %s[%lld] ;\n" , $3 ,$5) ;
                    printf("for(loopvariable = 0 ; loopvariable < %lld ; loopvariable++)\n" , $5) ;
                    printf("scanf(\"%s\" , &%s[loopvariable]);\n" , floatnum , $3) ;
                }
          |     INPUT FLOAT YSTRING '[' INTEGER ']'
                {
                    printf("for(loopvariable = 0 ; loopvariable < %lld ; loopvariable++)\n" , $5) ;
                    printf("scanf(\"%s\" , &%s[loopvariable]);\n" , floatnum , $3) ;
                }
          |     INPUT DOUBLE NSTRING '[' INTEGER ']'
                {
                    printf("double %s[%lld] ;\n" , $3 ,$5) ;
                    printf("for(loopvariable = 0 ; loopvariable < %lld ; loopvariable++)\n" , $5) ;
                    printf("scanf(\"%s\" , &%s[loopvariable]);\n" , doublenum , $3) ;
                }
          |     INPUT DOUBLE YSTRING '[' INTEGER ']'
                {
                    printf("for(loopvariable = 0 ; loopvariable < %lld ; loopvariable++)\n" , $5) ;
                    printf("scanf(\"%s\" , &%s[loopvariable]);\n" , doublenum , $3) ;
                }
          |     PRINT print_expr
                {   
                    printf("printf(\"\\n\");\n") ;
                }
          |     PRINT FIRST INTEGER NUMBER IVARIABLE '[' INTEGER ']'
                {
                    printf("for(loopvariable = 0 ; loopvariable < %lld ; loopvariable++)\n" , $3) ;
                    printf("printf(\"%s \" , %s[loopvariable]) ;\n\n" , integer , $5) ;
                }
          |     PRINT LAST INTEGER NUMBER IVARIABLE '[' INTEGER ']'
                {
                    printf("for(loopvariable = %lld ; loopvariable < %lld ; loopvariable++)\n" , ($7 - $3) , $7 ) ;
                    printf("printf(\"%s \" , %s[loopvariable]) ;\n\n" , integer , $5) ;
                }
          |     PRINT FIRST INTEGER NUMBER LIVARIABLE '[' INTEGER ']'
                {
                    printf("for(loopvariable = 0 ; loopvariable < %lld ; loopvariable++)\n" , $3) ;
                    printf("printf(\"%s \" , %s[loopvariable]) ;\n\n" , longinteger , $5) ;
                }
          |     PRINT LAST INTEGER NUMBER LIVARIABLE '[' INTEGER ']'
                {
                    printf("for(loopvariable = %lld ; loopvariable < %lld ; loopvariable++)\n" , ($7 - $3) , $7 ) ;
                    printf("printf(\"%s \" , %s[loopvariable]) ;\n\n" , longinteger , $5) ;
                }
          |     PRINT FIRST INTEGER NUMBER LLIVARIABLE '[' INTEGER ']'
                {
                    printf("for(loopvariable = 0 ; loopvariable < %lld ; loopvariable++)\n" , $3) ;
                    printf("printf(\"%s \" , %s[loopvariable]) ;\n\n" , longlonginteger , $5) ;
                }
          |     PRINT LAST INTEGER NUMBER LLIVARIABLE '[' INTEGER ']'
                {
                    printf("for(loopvariable = %lld ; loopvariable < %lld ; loopvariable++)\n" , ($7 - $3) , $7 ) ;
                    printf("printf(\"%s \" , %s[loopvariable]) ;\n\n" , longlonginteger , $5) ;
                }
          |     PRINT FIRST INTEGER NUMBER CVARIABLE '[' INTEGER ']'
                {
                    printf("for(loopvariable = 0 ; loopvariable < %lld ; loopvariable++)\n" , $3) ;
                    printf("printf(\"%s \" , %s[loopvariable]) ;\n\n" , character , $5) ;
                }
          |     PRINT LAST INTEGER NUMBER CVARIABLE '[' INTEGER ']'
                {
                    printf("for(loopvariable = %lld ; loopvariable < %lld ; loopvariable++)\n" , ($7 - $3) , $7 ) ;
                    printf("printf(\"%s \" , %s[loopvariable]) ;\n\n" , character , $5) ;
                } 
          |     PRINT FIRST INTEGER NUMBER FVARIABLE '[' INTEGER ']'
                {
                    printf("for(loopvariable = 0 ; loopvariable < %lld ; loopvariable++)\n" , $3) ;
                    printf("printf(\"%s \" , %s[loopvariable]) ;\n\n" , floatnum , $5) ;
                }
          |     PRINT LAST INTEGER NUMBER FVARIABLE '[' INTEGER ']'
                {
                    printf("for(loopvariable = %lld ; loopvariable < %lld ; loopvariable++)\n" , ($7 - $3) , $7 ) ;
                    printf("printf(\"%s \" , %s[loopvariable]) ;\n\n" , floatnum , $5) ;
                } 
          |     PRINT IVARIABLE '[' INTEGER ']'
                {
                    printf("for(loopvariable = 0 ; loopvariable < %lld ; loopvariable++)\n" , $4) ;
                    printf("printf(\"%s \" , %s[loopvariable]) ;\n\n" , integer , $2) ;
                }
          |     PRINT LIVARIABLE '[' INTEGER ']'
                {
                    printf("for(loopvariable = 0 ; loopvariable < %lld ; loopvariable++)\n" , $4) ;
                    printf("printf(\"%s \" , %s[loopvariable]) ;\n\n" , longinteger , $2) ;
                }
          |     PRINT LLIVARIABLE '[' INTEGER ']'
                {
                    printf("for(loopvariable = 0 ; loopvariable < %lld ; loopvariable++)\n" , $4) ;
                    printf("printf(\"%s \" , %s[loopvariable]) ;\n\n" , longlonginteger , $2) ;
                }
          |     PRINT CVARIABLE '[' INTEGER ']'
                {
                    printf("for(loopvariable = 0 ; loopvariable < %lld ; loopvariable++)\n" , $4) ;
                    printf("printf(\"%s \" , %s[loopvariable]) ;\n\n" , character , $2) ;
                }
          |     PRINT FVARIABLE '[' INTEGER ']'
                {
                    printf("for(loopvariable = 0 ; loopvariable < %lld ; loopvariable++)\n" , $4) ;
                    printf("printf(\"%s \" , %s[loopvariable]) ;\n\n" , floatnum , $2) ;
                }
          |     PRINT IVARIABLE '[' INTEGER ':' INTEGER ']'
                {
                    printf("for(loopvariable = %lld ; loopvariable < %lld ; loopvariable++)\n" , $4 , $6) ;
                    printf("printf(\"%s \" , %s[loopvariable]) ;\n\n" , integer , $2) ;
                }
          |     PRINT LIVARIABLE '[' INTEGER ':' INTEGER ']'
                {
                    printf("for(loopvariable = %lld ; loopvariable < %lld ; loopvariable++)\n" , $4 , $6) ;
                    printf("printf(\"%s \" , %s[loopvariable]) ;\n\n" , longinteger , $2) ;
                }
          |     PRINT LLIVARIABLE '[' INTEGER ':' INTEGER ']'
                {
                    printf("for(loopvariable = %lld ; loopvariable < %lld ; loopvariable++)\n" , $4 , $6) ;
                    printf("printf(\"%s \" , %s[loopvariable]) ;\n\n" , longlonginteger , $2) ;
                }
          |     PRINT CVARIABLE '[' INTEGER ':' INTEGER ']'
                {
                    printf("for(loopvariable = %lld ; loopvariable < %lld ; loopvariable++)\n" , $4 , $6) ;
                    printf("printf(\"%s \" , %s[loopvariable]) ;\n\n" , character , $2) ;
                }
          |     PRINT FVARIABLE '[' INTEGER ':' INTEGER ']'
                {
                    printf("for(loopvariable = %lld ; loopvariable < %lld ; loopvariable++)\n" , $4 , $6) ;
                    printf("printf(\"%s \" , %s[loopvariable]) ;\n\n" , floatnum , $2) ;
                }           
          |     IF CONDITION 
                {
                    printf("if (%s){\n" , $2) ;
                }
                prog ENDIF
                { 
                    printf("}\n") ;
                }
          |     ELSE IF CONDITION
                {
                    printf("else if (%s) {\n" , $3) ;
                }
                prog ENDELSE
                { 
                    printf("}\n") ;
                }
          |     ELSE
                {
                    printf("else {\n") ;
                }
                prog ENDELSE
                { 
                    printf("}\n") ;
                }
          |     DO WHILE CONDITION
                {
                    printf("while(%s) {\n" , $3) ;
                }
                prog ENDDO
                {
                    printf("}\n") ;
                } 
          |     FOR STRING STRING INTEGER STRING INTEGER STRING INTEGER
                {
                    printf("int %s ;\n" , $2) ;
                    printf("for (%s = %lld ; %s < %lld ; %s += %lld){\n" , $2 , $4 , $2 , $6 , $2 , $8) ;
                }
                prog ENDFOR
                {
                    printf("}\n") ;
                }
          |     FOR STRING STRING INTEGER STRING STRING STRING INTEGER
                {
                    printf("int %s ;\n" , $2) ;
                    printf("for (%s = %lld ; %s < %s ; %s += %lld){\n" , $2 , $4 , $2 , $6 , $2 , $8) ;
                }
                prog ENDFOR
                {
                    printf("}\n") ;
                }
          |     STRING '=' MIN
                {
                    printf("min = 20000;\n") ;
                }
                min_num
                {   
                    printf("%s = min;\n" , $1) ;
                }
          |     STRING '=' MAX
                {
                    printf("max = -20000;\n") ;
                }
                max_num
                {   
                    printf("%s = max;\n" , $1) ;
                }
          |     STRING '=' FACTORIAL INTEGER
                {
                    printf("%s = 1 ;\n" , $1) ;
                    printf("for(loopvariable = 0 ; loopvariable < %lld ; loopvariable++){\n" , $4) ;
                    printf("%s = %s*loopvariable ;\n }\n" , $1 , $1) ;
                }                    
          |     STRING '=' CALL FSTRING
                {
                    printf("%s = %s(" , $1 , $4) ;
                }
                paramtr
                {
                    printf(");\n") ;
                }
          |     CALL FSTRING
                {
                    printf("%s(" , $2 ) ;
                }
                paramtr
                {
                    printf(");\n") ;
                }
          |     RETURN INTEGER
                {
                    printf("%s %lld;\n" , $1 , $2) ;
                }
          |     RETURN STRING
                {
                    printf("%s %s;\n" , $1 , $2) ;
                }
          |     RETURN FLOATN
                {
                    printf("%s %s;\n" , $1 , $2) ;
                }
          |     RETURN CHARN
                {
                    printf("%s %s;\n" , $1 , $2) ;
                }
          |     RETURN DOUBLEN
                {
                    printf("%s %s;\n" , $1 , $2) ;
                }
          |     INT STRING '[' INTEGER ']'
                {
                    printf("%s %s[%lld] ;\n" , $1 , $2 ,$4) ;
                }
          |     INT STRING '[' INTEGER ']' '='
                {
                    printf("%s %s[%lld] = {" , $1 , $2 , $4) ;
                }
                arraynum
                {
                    printf("} ;\n") ;
                }
          |     LONGINT STRING '[' INTEGER ']'
                {
                    printf("%s %s[%lld] ;\n" , $1 , $2 ,$4) ;
                }
          |     LONGINT STRING '[' INTEGER ']' '='
                {
                    printf("%s %s[%lld] = {" , $1 , $2 , $4) ;
                }
                arraynum
                {
                    printf("} ;\n") ;
                }
          |     LONGLONGINT STRING '[' INTEGER ']'
                {
                    printf("%s %s[%lld] ;\n" , $1 , $2 ,$4) ;
                }
          |     LONGLONGINT STRING '[' INTEGER ']' '='
                {
                    printf("%s %s[%lld] = {" , $1 , $2 , $4) ;
                }
                arraynum
                {
                    printf("} ;\n") ;
                }
          |     STRING '[' INTEGER ']' '=' 
                {
                    printf("%s[%lld] = {" , $1 , $3) ;
                }
                arraynum
                {
                    printf("} ;\n") ;
                }
          |     FLOAT STRING '[' INTEGER ']'
                {
                    printf("%s %s[%lld] ;\n" , $1 , $2 ,$4) ;
                }
          |     FLOAT STRING '[' INTEGER ']' '='
                {
                    printf("%s %s[%lld] = {" , $1 , $2 , $4) ;
                }
                arrayfloat
                {
                    printf("} ;\n") ;
                }
          |     STRING '[' INTEGER ']' '=' 
                {
                    printf("%s[%lld] = {" , $1 , $3) ;
                }
                arrayfloat
                {
                    printf("} ;\n") ;
                }
          |     DOUBLE STRING '[' INTEGER ']'
                {
                    printf("%s %s[%lld] ;\n" , $1 , $2 ,$4) ;
                }
          |     DOUBLE STRING '[' INTEGER ']' '='
                {
                    printf("%s %s[%lld] = {" , $1 , $2 , $4) ;
                }
                arrayfloat
                {
                    printf("} ;\n") ;
                }
          |     CHAR STRING '[' INTEGER ']'
                {
                    printf("%s %s[%lld] ;\n" , $1 , $2 ,$4) ;
                }
          |     CHAR STRING '[' INTEGER ']' '='
                {
                    printf("%s %s[%lld] = {" , $1 , $2 , $4) ;
                }
                arraychar
                {
                    printf("} ;\n") ;
                }
          |     STRING '[' INTEGER ']' '=' 
                {
                    printf("%s[%lld] = {" , $1 , $3) ;
                }
                arraychar
                {
                    printf("} ;\n") ;
                }
          |     SORT FIRST INTEGER NUMBER ARRAY STRING INTEGER
                {
                    printf("for(loopvariable1 = 1; loopvariable1 < %lld; loopvariable1++){ \n" , $3) ;
                    printf("for(loopvariable = 1; loopvariable < %lld; loopvariable++){ \n" , $3) ;
                    printf("if(%s[loopvariable - 1] > %s[loopvariable]){ \n" , $6 , $6) ;
                    printf("temp = %s[loopvariable -1] ;\n" , $6) ;
                    printf("%s[loopvariable - 1] = %s[loopvariable] ;\n" , $6 , $6) ;
                    printf("%s[loopvariable] = temp ;\n" , $6) ;
                    printf("}\n}\n}\n") ;
                }
          |     SORT LAST INTEGER NUMBER ARRAY STRING INTEGER
                {
                    printf("for(loopvariable1 = %lld ; loopvariable1 > %lld ; loopvariable1--){ \n" , ($7 - 1) , ($7 - $3)) ;
                    printf("for(loopvariable = %lld ; loopvariable > %lld ; loopvariable--){ \n" , ($7 - 1) , ($7 - $3)) ;
                    printf("if(%s[loopvariable - 1] > %s[loopvariable]){ \n" , $6 , $6) ;
                    printf("temp = %s[loopvariable -1] ;\n" , $6) ;
                    printf("%s[loopvariable - 1] = %s[loopvariable] ;\n" , $6 , $6) ;
                    printf("%s[loopvariable] = temp ;\n" , $6) ;
                    printf("}\n}\n}\n") ;
                }
          |     SORT ARRAY STRING INTEGER
                {
                    printf("for(loopvariable1 = 1; loopvariable1 < %lld; loopvariable1++){ \n" , $4) ;
                    printf("for(loopvariable = 1; loopvariable < %lld; loopvariable++){ \n" , $4) ;
                    printf("if(%s[loopvariable - 1] > %s[loopvariable]){ \n" , $3 , $3) ;
                    printf("temp = %s[loopvariable -1] ;\n" , $3) ;
                    printf("%s[loopvariable - 1] = %s[loopvariable] ;\n" , $3 , $3) ;
                    printf("%s[loopvariable] = temp ;\n" , $3) ;
                    printf("}\n}\n}\n") ;
                }
          |     INPUT CHARACTER STRING
                {
                    printf("char %s ;\n" , $3) ;
                    printf("scanf(\"%s\" , &%s);\n" , character , $3) ;
                }
          |     INPUT DOUBLENUM STRING
                {
                    printf("double %s ;\n" , $3) ;
                    printf("scanf(\"%s\" , &%s);\n" , doublenum , $3) ;
                }
          |     INPUT FLOATNUM STRING
                {
                    printf("float %s ;\n" , $3) ;
                    printf("scanf(\"%s\" , &%s);\n" , floatnum , $3) ;
                }
          |     INPUT INTEGER STRING
                {
                    printf("int %s ;\n" , $3) ;
                    printf("scanf(\"%s\" , &%s);\n" , integer , $3) ;
                }
          |     INPUT LONGINTEGER STRING
                {
                    printf("int %s ;\n" , $3) ;
                    printf("scanf(\"%s\" , &%s);\n" , longinteger , $3) ;
                }
          |     INPUT LONGLONGINTEGER STRING
                {
                    printf("int %s ;\n" , $3) ;
                    printf("scanf(\"%s\" , &%s);\n" , longlonginteger , $3) ;
                }
          |     STRUCTSTRING STRING
                {
                    printf("struct %s %s ;\n" , $1 , $2) ;
                }
          |     STRUCTSTRING STRING '='
                {
                    printf("struct %s %s = {" , $1 , $2) ;
                }
                struct_val
                {
                    printf("};\n") ;
                }
          |      STRUCTSTRING POINTER STRING
                 {
                     printf("struct %s *%s ;\n" , $1 , $3) ;
                 }
          |      STRING IN STRING '=' INTEGER
                 {
                     printf("%s.%s = %lld ;\n" , $3 , $1 , $5) ;
                 }
          |      STRING IN STRING '=' CHARN
                 {
                     printf("%s.%s = %s ;\n" , $3 , $1 , $5) ;
                 }
          |      STRING IN STRING '=' STRING
                 {
                     printf("%s.%s = %s ;\n" , $3 , $1 , $5) ;
                 }
          |      STRING IN STRING '=' FLOATN
                 {
                     printf("%s.%s = %s ;\n" , $3 , $1 , $5) ;
                 }
          |      STRING IN STRING '=' DOUBLEN
                 {
                     printf("%s.%s = %s ;\n" , $3 , $1 , $5) ;
                 }
          |      STRING IN PTRSTRING '=' INTEGER
                 {
                     printf("%s->%s = %lld ;\n" , $3 , $1 , $5) ;
                 }
          |      STRING IN PTRSTRING '=' CHARN
                 {
                     printf("%s->%s = %s ;\n" , $3 , $1 , $5) ;
                 }
          |      STRING IN PTRSTRING '=' STRING
                 {
                     printf("%s->%s = %s ;\n" , $3 , $1 , $5) ;
                 }
          |      STRING IN PTRSTRING '=' FLOATN
                 {
                     printf("%s->%s = %s ;\n" , $3 , $1 , $5) ;
                 }
          |      STRING IN PTRSTRING '=' DOUBLEN
                 {
                     printf("%s->%s = %s ;\n" , $3 , $1 , $5) ;
                 }
          |      STRING IN PTRSTRING '=' STRING IN PTRSTRING
                 {
                     printf("%s->%s = %s->%s ;\n" , $3 , $1 , $7 , $5) ;
                 }
          |      STRING IN STRING IN PTRSTRING '=' INTEGER
                 {
                     printf("%s->%s->%s = %lld ;\n" ,$5 , $3 , $1 , $7) ;
                 }
          |      STRING IN STRING IN PTRSTRING '=' CHARN
                 {
                     printf("%s->%s->%s = %s ;\n" ,$5 , $3 , $1 , $7) ;
                 }
          |      STRING IN STRING IN PTRSTRING '=' STRING
                 {
                     printf("%s->%s->%s = %s ;\n" ,$5 , $3 , $1 , $7) ;
                 }
          |      STRING IN STRING IN PTRSTRING '=' FLOATN
                 {
                     printf("%s->%s->%s = %s ;\n" ,$5 , $3 , $1 , $7) ;
                 }
          |      STRING IN STRING IN PTRSTRING '=' DOUBLEN
                 {
                     printf("%s->%s->%s = %s ;\n" ,$5 , $3 , $1 , $7) ;
                 }
          |      STRING IN STRING IN PTRSTRING '=' STRING IN PTRSTRING
                 {
                     printf("%s->%s->%s = %s->%s ;\n" ,$5 , $3 , $1 , $9 , $7) ;
                 }
          |      STRING IN STRING IN PTRSTRING '=' STRING IN STRING IN PTRSTRING
                 {
                     printf("%s->%s->%s = %s->%s->%s ;\n" ,$5 , $3 , $1 , $11 , $9 , $7) ;
                 }
          |      INT POINTER STRING
                 {
                     printf("int *%s ;\n" , $3) ;
                 }
          |      LONGINT POINTER STRING
                 {
                     printf("long int *%s ;\n" , $3) ;
                 }
          |      LONGLONGINT POINTER STRING
                 {
                     printf("long long int *%s ;\n" , $3) ;
                 }
          |      CHAR POINTER STRING
                 {
                     printf("char *%s ;\n" , $3) ;
                 }
          |      FLOAT POINTER STRING
                 {
                     printf("float *%s ;\n" , $3) ;
                 }
          |      DOUBLE POINTER STRING
                 {
                     printf("double *%s ;\n" , $3) ;
                 }
          |      STRING POINTS STRING
                 {
                     printf("%s = &%s ;\n" , $1 , $3) ;
                 }
          |      STRING POINTS INTEGER
                 {
                     printf("*%s = %lld ;\n" , $1 , $3) ;
                 }
          |      STRING POINTS CHARN
                 {
                     printf("*%s = %s ;\n" , $1 , $3) ;
                 }
          |      STRING POINTS FLOATN
                 {
                     printf("*%s = %s ;\n" , $1 , $3) ;
                 }
          |      STRING '=' EXPR
                 {
                     printf("%s = %s ;\n" , $1 , $3) ;
                 }
          |      STRING '=' STRING
                 {
                     printf("%s = %s ;\n" , $1 , $3) ;
                 }
          |      MALLOC MEMORY STRING POINTER STRING
                 {
                     printf("%s = (struct %s *)malloc(sizeof(struct %s)) ;\n" , $5 , $3 , $3) ;
                 }
          ;


struct_val:     INTEGER
                {
                    printf("%lld" , $1) ;
                }
          |     CHARN
                {
                    printf("%s" , $1) ;
                }
          |     FLOATN
                {
                    printf("%s" , $1) ;
                }
          |     struct_val INTEGER  
                {
                    printf(",%lld" , $2) ;
                }
          |     struct_val CHARN 
                {
                    printf(",%s" , $2) ;
                }
          |     struct_val FLOATN
                {
                    printf(",%s" , $2) ;
                }
          ;


arraynum  :     arraynum INTEGER
                {
                    printf(",%lld" , $2) ;
                }
          |     INTEGER
                {
                    printf("%lld" , $1) ;
                }
          |    
          ;

arrayfloat:     arrayfloat FLOATN
                {
                    printf(",%s" , $2) ;
                }
          |     FLOATN
                {
                    printf("%s" , $1) ;
                }
          |    
          ;
          
arraychar :     arraychar CHARN
                {
                    printf(",%s" , $2) ;
                }
          |     CHARN
                {
                    printf("%s" , $1) ;
                }
          ;     

            
min_num   :     min_num MSTRING
                {   
                    printf("if(%s < min) min = %s;\n" , $2 , $2) ;
                }
          |     MSTRING
                {
                    printf("if(%s < min) min = %s;\n" , $1 , $1) ;
                }
          ;



max_num   :     max_num MSTRING
                {   
                    printf("if(%s > max) max = %s;\n" , $2 , $2) ;
                }
          |     MSTRING
                {
                    printf("if(%s > max) max = %s;\n" , $1 , $1) ;
                }
          ;

paramtr   :     FSTRING
                {
                    printf("%s" , $1) ;
                }
          |     FINTEGER
                {
                    printf("%lld" , $1) ;
                }
          |     paramtr FSTRING
                {
                    printf(",%s" , $2) ;
                }
          |     paramtr FINTEGER
                {
                    printf(",%lld" , $2) ;
                }
          |
          ;
 

print_expr:     PRSTRING
                {
                    printf("printf(\"%s \");\n" , $1) ;
                }
          |     IVARIABLE
                {
                    printf("printf(\"%s \", %s);\n" , integer , $1) ;
                }
          |     LIVARIABLE
                {
                    printf("printf(\"%s \", %s);\n" , longinteger , $1) ;
                }
          |     LLIVARIABLE
                {
                    printf("printf(\"%s \", %s);\n" , longlonginteger , $1) ;
                }
          |     CVARIABLE
                {
                    printf("printf(\"%s \", %s);\n" , character , $1) ;
                }
          |     FVARIABLE
                {
                    printf("printf(\"%s \", %s);\n" , floatnum , $1) ;
                }
          |     DVARIABLE
                {
                    printf("printf(\"%s \", %s);\n" , doublenum , $1) ;
                }
          |     print_expr PRSTRING
                {
                    printf("printf(\"%s \");\n" , $2) ;
                }
          |     print_expr IVARIABLE
                {
                    printf("printf(\"%s \", %s);\n" , integer , $2) ;
                }
          |     print_expr LIVARIABLE
                {
                    printf("printf(\"%s \", %s);\n" , longinteger , $2) ;
                }
          |     print_expr LLIVARIABLE
                {
                    printf("printf(\"%s \", %s);\n" , longlonginteger , $2) ;
                }
          |     print_expr CVARIABLE
                {
                    printf("printf(\"%s \", %s);\n" , character , $2) ;
                }
          |     print_expr FVARIABLE
                {
                    printf("printf(\"%s \", %s);\n" , floatnum , $2) ;
                }
          |     print_expr DVARIABLE
                {
                    printf("printf(\"%s \", %s);\n" , doublenum , $2) ;
                }
          ;

/*variable  :     IVARIABLE
                {
                    printf("printf(\"%s \", %s);\n" , integer , $1) ;
                }
          |     CVARIABLE
                {
                    printf("printf(\"%s \", %s);\n" , character , $1) ;
                }
          |     FVARIABLE
                {
                    printf("printf(\"%s \", %s);\n" , floatnum , $1) ;
                }
          ;*/
          
          
%%

main()
{
        printf("#include<stdio.h>\n\n") ;
        printf("int min ;\n") ;
        printf("int max ;\n") ;
        printf("int temp ;\n") ;
        printf("int loopvariable ;\n") ; 
        printf("int loopvariable1 ;\n\n") ; 
	yyparse();
	} 

void yyerror(char *s){
    fprintf(stderr, "%s\n" , s) ;
}