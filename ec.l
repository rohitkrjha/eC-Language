%{

  #include "y.tab.h"
  #include<stdio.h>
  #include<string.h>
  #include<stdlib.h>

  enum{
    LOOKUP = -6 ,
    INTDECLARATION ,
    CHARDECLARATION ,
    FLOATDECLARATION ,
    DOUBLEDECLARATION ,
    ARRAYDECLARATION ,
    STRUCTDECLARATION ,
    SUBSTRUCTDECLARATION ,
    SUBSTRUCTPTRDECLARATION ,
    POINTERDECLARATION ,
    INTFUNCTION ,
    CHARFUNCTION ,
    FLOATFUNCTION ,
    VOIDFUNCTION ,
    DOUBLEFUNCTION ,
    LLINTFUNCTION ,
    LINTFUNCTION ,
    LONGINTDECLARATION ,
    LONGLONGINTDECLARATION
  };

  int state ;
  int add_word(int type, char *word, int size) ;
  int lookup_word(char *word) ;

%}

%option noyywrap
%s P PRINTSTRING CONDT FORCONDT SCAN IVARNAME FVARNAME CVARNAME DVARNAME ARRAYINITIALIZATION MINIMUM MAXIMUM SORTING CALLING IDECLARATION LIDECLARATION LLIDECLARATION MALLOCATION PDECLARATION STRUCTVALDEC STRUCTSTRPTR FDECLARATION DDECLARATION CDECLARATION STRUCTURE PARAFNNAME IFNNAME CFNNAME FFNNAME VFNNAME DFNNAME FUNCTION STRUCTSTR LLIFNAME LIFNAME LIVARNAME LLIVARNAME LLIFNNAME LIFNNAME
DIGIT [0-9]
LETTERS [a-zA-Z]
LETTER [a-zA-Z0-9]

%%

[ \t]                                     { ;}
[;]                                       {BEGIN INITIAL ;}
<FUNCTION>"int"                           {strcpy(yylval.str , yytext) ; BEGIN IFNNAME ; return INT ;}
<FUNCTION>"long long int"                 {strcpy(yylval.str , yytext) ; BEGIN LLIFNNAME ; return LONGLONGINT ;}
<FUNCTION>"long int"                      {strcpy(yylval.str , yytext) ; BEGIN LIFNNAME ; return LONGINT ;}
<FUNCTION>"char"                          {strcpy(yylval.str , yytext) ; BEGIN CFNNAME ; return CHAR ;}
<FUNCTION>"float"                         {strcpy(yylval.str , yytext) ; BEGIN FFNNAME ; return FLOAT ;}
<FUNCTION>"double"                        {strcpy(yylval.str , yytext) ; BEGIN DFNNAME ; return DOUBLE ;}
<FUNCTION>"void"                          {strcpy(yylval.str , yytext) ; BEGIN VFNNAME ; return VOID ;}
<FUNCTION>"("|","|")"                     { ;}
<IFNNAME>[a-zA-Z]{LETTER}*                {    
                                              add_word(INTFUNCTION , yytext , 0) ; 
                                              strcpy(yylval.str , yytext) ; 
                                              BEGIN PARAFNNAME ; 
                                              return STRING ;
                                          }
<LLIFNNAME>[a-zA-Z]{LETTER}*              {    
                                              add_word(LLINTFUNCTION , yytext , 0) ; 
                                              strcpy(yylval.str , yytext) ; 
                                              BEGIN PARAFNNAME ; 
                                              return STRING ;
                                          }
<LIFNNAME>[a-zA-Z]{LETTER}*               {    
                                              add_word(LINTFUNCTION , yytext , 0) ; 
                                              strcpy(yylval.str , yytext) ; 
                                              BEGIN PARAFNNAME ; 
                                              return STRING ;
                                          }
<CFNNAME>[a-zA-Z]{LETTER}*                {
                                              add_word(CHARFUNCTION , yytext , 0) ; 
                                              strcpy(yylval.str , yytext) ; 
                                              BEGIN PARAFNNAME ;
                                              return STRING ;
                                          }
<FFNNAME>[a-zA-Z]{LETTER}*                {
                                              add_word(FLOATFUNCTION , yytext , 0) ; 
                                              strcpy(yylval.str , yytext) ; 
                                              BEGIN PARAFNNAME ;
                                              return STRING ;
                                          }
<VFNNAME>[a-zA-Z]{LETTER}*                {
                                              add_word(VOIDFUNCTION , yytext , 0) ; 
                                              strcpy(yylval.str , yytext) ; 
                                              BEGIN PARAFNNAME ;
                                              return STRING ;
                                          }
<DFNNAME>[a-zA-Z]{LETTER}*                {
                                              add_word(DOUBLEFUNCTION , yytext , 0) ; 
                                              strcpy(yylval.str , yytext) ; 
                                              BEGIN PARAFNNAME ;
                                              return STRING ;
                                          }
<PARAFNNAME>"int"                         {strcpy(yylval.str , yytext) ; return INT ;}
<PARAFNNAME>"char"                        {strcpy(yylval.str , yytext) ; return CHAR ;}
<PARAFNNAME>"float"                       {strcpy(yylval.str , yytext) ; return FLOAT ;}
<PARAFNNAME>"double"                      {strcpy(yylval.str , yytext) ; return DOUBLE ;}
<PARAFNNAME>"long int"                    {strcpy(yylval.str , yytext) ; return LONGINT ;}
<PARAFNNAME>"long long int"               {strcpy(yylval.str , yytext) ; return LONGLONGINT ;}
<PARAFNNAME>"start"                       {strcpy(yylval.str , yytext) ; BEGIN INITIAL ; return START ;}
<INITIAL>"endfunction"                    {strcpy(yylval.str , yytext) ; return END ;}
<PARAFNNAME>[a-zA-Z]{LETTER}*             {strcpy(yylval.str , yytext) ; return STRING ;}
<SCAN>"float"                             {strcpy(yylval.str , yytext) ; BEGIN FVARNAME ; return FLOAT ;}
<SCAN>"double"                            {strcpy(yylval.str , yytext) ; BEGIN DVARNAME ; return DOUBLE ;}
<SCAN>"int"                               {strcpy(yylval.str , yytext) ; BEGIN IVARNAME ; return INT ;}
<SCAN>"long int"                          {strcpy(yylval.str , yytext) ; BEGIN LIVARNAME ; return LONGINT ;}
<SCAN>"long long int"                     {strcpy(yylval.str , yytext) ; BEGIN LLIVARNAME ; return LONGLONGINT ;}
<SCAN>"char"                              {strcpy(yylval.str , yytext) ; BEGIN CVARNAME ; return CHAR ;}
<SCAN>[a-zA-Z][a-zA-Z0-9]*                {
                                              strcpy(yylval.str , yytext) ;
                                              if(lookup_word(yytext) == -5)
                                                   return IVARIABLE ;
                                              if(lookup_word(yytext) == -4)
                                                   return CVARIABLE ;
                                              if(lookup_word(yytext) == -3)
                                                   return FVARIABLE ;
                                              if(lookup_word(yytext) == -2)
                                                   return DVARIABLE ;
                                              if(lookup_word(yytext) == 11)
                                                   return LIVARIABLE ;
                                              if(lookup_word(yytext) == 12)
                                                   return LLIVARIABLE ;
                                          }
<SCAN>"["                                 {   return yytext[0] ;}
<SCAN>"]"                                 {   return yytext[0] ;}
<SCAN>","|"."|"("|")"                     { ;}
<SCAN>{DIGIT}+                            {   yylval.num = atoll(yytext) ; return INTEGER ;}
<IVARNAME>[a-zA-Z][a-zA-Z0-9]*            {
                                              strcpy(yylval.str , yytext) ;
                                              if(lookup_word(yytext) == -5)
                                                   return YSTRING ; 
                                              else {
                                                  add_word(INTDECLARATION , yytext , 0) ;
                                                  return NSTRING ;
                                                  }
                                          }
<IVARNAME>"["                             {return yytext[0] ;}
<IVARNAME>"]"                             {return yytext[0] ;}
<IVARNAME>","|"."|"("|")"                 { ;}
<IVARNAME>{DIGIT}+                        {yylval.num = atoll(yytext) ; return INTEGER ;}
<LIVARNAME>[a-zA-Z][a-zA-Z0-9]*           {
                                              strcpy(yylval.str , yytext) ;
                                              if(lookup_word(yytext) == 11)
                                                   return YSTRING ; 
                                              else {
                                                  add_word(LONGINTDECLARATION , yytext , 0) ;
                                                  return NSTRING ;
                                                  }
                                          }
<LIVARNAME>"["                            {return yytext[0] ;}
<LIVARNAME>"]"                            {return yytext[0] ;}
<LIVARNAME>","|"."|"("|")"                { ;}
<LIVARNAME>{DIGIT}+                       {yylval.num = atoll(yytext) ; return INTEGER ;}
<LLIVARNAME>[a-zA-Z][a-zA-Z0-9]*          {
                                              strcpy(yylval.str , yytext) ;
                                              if(lookup_word(yytext) == 12)
                                                   return YSTRING ; 
                                              else {
                                                  add_word(LONGLONGINTDECLARATION , yytext , 0) ;
                                                  return NSTRING ;
                                                  }
                                          }
<LLIVARNAME>"["                           {return yytext[0] ;}
<LLIVARNAME>"]"                           {return yytext[0] ;}
<LLIVARNAME>{DIGIT}+                      {yylval.num = atoll(yytext) ; return INTEGER ;}
<FVARNAME>[a-zA-Z][a-zA-Z0-9]*            {
                                              strcpy(yylval.str , yytext) ;
                                              if(lookup_word(yytext) == -3)
                                                   return YSTRING ; 
                                              else {
                                                  add_word(FLOATDECLARATION , yytext , 0) ;
                                                  return NSTRING ;
                                                  }
                                          }
<FVARNAME>"["                             {return yytext[0] ;}
<FVARNAME>"]"                             {return yytext[0] ;}
<FVARNAME>{DIGIT}+                        {yylval.num = atoll(yytext) ; return INTEGER ;}
<DVARNAME>[a-zA-Z][a-zA-Z0-9]*            {
                                              strcpy(yylval.str , yytext) ;
                                              if(lookup_word(yytext) == -2)
                                                   return YSTRING ; 
                                              else {
                                                  add_word(DOUBLEDECLARATION , yytext , 0) ;
                                                  return NSTRING ;
                                                  }
                                          }
<DVARNAME>"["                             {return yytext[0] ;}
<DVARNAME>"]"                             {return yytext[0] ;}
<DVARNAME>{DIGIT}+                        {yylval.num = atoll(yytext) ; return INTEGER ;}
<CVARNAME>[a-zA-Z][a-zA-Z0-9]*            {
                                              strcpy(yylval.str , yytext) ;
                                              if(lookup_word(yytext) == -4)
                                                   return YSTRING ;
                                              else {
                                                  add_word(CHARDECLARATION , yytext , 0) ;
                                                  return NSTRING ;
                                                  }
                                          }
<CVARNAME>"["                             {return yytext[0] ;}
<CVARNAME>"]"                             {return yytext[0] ;}
<CVARNAME>{DIGIT}+                        {yylval.num = atoll(yytext) ; return INTEGER ;}
"print"                                   {strcpy(yylval.str , yytext) ; BEGIN P ; return PRINT ;}
"long int"                                {strcpy(yylval.str , yytext) ; BEGIN LIDECLARATION ; return LONGINT ;}
"long long int"                           {strcpy(yylval.str , yytext) ; BEGIN LLIDECLARATION ; return LONGLONGINT ;}
"int"                                     {strcpy(yylval.str , yytext) ; BEGIN IDECLARATION ; return INT ;}
"float"                                   {strcpy(yylval.str , yytext) ; BEGIN FDECLARATION ; return FLOAT ;}
"double"                                  {strcpy(yylval.str , yytext) ; BEGIN DDECLARATION ; return DOUBLE ;}
"char"                                    {strcpy(yylval.str , yytext) ; BEGIN CDECLARATION ; return CHAR ;}
"def"                                     {strcpy(yylval.str , yytext) ; BEGIN FUNCTION ; return DEF ;}
"call"                                    {strcpy(yylval.str , yytext) ; BEGIN CALLING ; return CALL ;}
"if"                                      {strcpy(yylval.str , yytext) ; BEGIN CONDT ; return IF ;}
"else"                                    {strcpy(yylval.str , yytext) ; return ELSE ;}
"for"                                     {strcpy(yylval.str , yytext) ; BEGIN FORCONDT ; return FOR ;}
"input"|"enter"                           {strcpy(yylval.str , yytext) ; BEGIN SCAN ; return INPUT ;}
"start"                                   {strcpy(yylval.str , yytext) ; return START ;}
"end"                                     {strcpy(yylval.str , yytext) ; return END ;}
"main"                                    {strcpy(yylval.str , yytext) ; return MAIN ;}
"return"                                  {strcpy(yylval.str , yytext) ; return RETURN ;}
"endif"                                   {strcpy(yylval.str , yytext) ; return ENDIF ;}
"endelse"                                 {strcpy(yylval.str , yytext) ; return ENDELSE ;}
"endfor"                                  {strcpy(yylval.str , yytext) ; return ENDFOR ;}
"do"                                      {strcpy(yylval.str , yytext) ; return DO ;}
"while"                                   {strcpy(yylval.str , yytext) ; BEGIN CONDT ; return WHILE ;}
"enddo"                                   {strcpy(yylval.str , yytext) ; return ENDDO ;} 
"min"                                     {strcpy(yylval.str , yytext) ; BEGIN MINIMUM ; return MIN ;}
"max"                                     {strcpy(yylval.str , yytext) ; BEGIN MAXIMUM ; return MAX ;}
"factorial"                               {strcpy(yylval.str , yytext) ; return FACTORIAL ;}
"sort"                                    {strcpy(yylval.str , yytext) ; BEGIN SORTING ; return SORT ;}
"struct"                                  {strcpy(yylval.str , yytext) ; BEGIN STRUCTURE ; return STRUCT ;}
"points"                                  {strcpy(yylval.str , yytext) ; return POINTS ;}
"break"|"leave loop"                      {strcpy(yylval.str , yytext) ; return BREAK ;}
"continue"|"skip next"                    {strcpy(yylval.str , yytext) ; return CONTINUE ;}
"exit"|"finish"                           {strcpy(yylval.str , yytext) ; return EXIT ;}
"allocate"|"provide"|"give"               {strcpy(yylval.str , yytext) ; BEGIN MALLOCATION ; return MALLOC ;}
<MALLOCATION>"memory"|"space"             {strcpy(yylval.str , yytext) ; return MEMORY ;}
<MALLOCATION>"to"|","|"."|"("|")"         { ;}
<MALLOCATION>"pointer"                    {strcpy(yylval.str , yytext) ; return POINTER ;}
<MALLOCATION>[a-zA-Z][a-zA-Z0-9]*         {strcpy(yylval.str , yytext) ; return STRING ;}
<CALLING>[a-zA-Z][a-zA-Z0-9]*             {strcpy(yylval.str , yytext) ; return FSTRING ;}
<CALLING>{DIGIT}+                         {yylval.num = atoll(yytext) ; return FINTEGER ;}
<CALLING>"("|")"|","                      { ;}
<STRUCTURE>[a-zA-Z][a-zA-Z0-9]*           {strcpy(yylval.str , yytext) ; add_word(STRUCTDECLARATION , yytext , 0) ; BEGIN INITIAL ; return STRING ;}
<SORTING>"first"                          {strcpy(yylval.str , yytext) ; return FIRST ;}
<SORTING>"last"                           {strcpy(yylval.str , yytext) ; return LAST ;}
<SORTING>[0-9]+			           {yylval.num = atoll(yytext) ; return INTEGER ;}
<SORTING>"number"|"numbers"               {strcpy(yylval.str , yytext) ; return NUMBER ;}
<SORTING>"array"                          {strcpy(yylval.str , yytext) ; return ARRAY ;}
<SORTING>"["|"]"                          { ;}
<SORTING>"of"|","|"."|")"|"("             { ;}
<SORTING>"in"                             { ;}
<SORTING>[a-zA-Z][a-zA-Z0-9]*             {strcpy(yylval.str , yytext) ; return STRING ;}
<P>"first"                                {strcpy(yylval.str , yytext) ; return FIRST ;}
<P>"last"                                 {strcpy(yylval.str , yytext) ; return LAST ;}
<P>[0-9]+                                 {yylval.num = atoll(yytext) ; return INTEGER ;}
<P>"element"|"elements"                   {strcpy(yylval.str , yytext) ; return NUMBER ;}
<P>"of"|"in"                              { ;}
<P>"["                                    {return yytext[0] ;}
<P>"]"                                    {return yytext[0] ;}
<P>":"                                    {return yytext[0] ;}
<P>"\""                                   {BEGIN PRINTSTRING ;}
<P>[a-zA-Z][a-zA-Z0-9]*                   {
                                              strcpy(yylval.str , yytext) ;
                                              if(lookup_word(yytext) == -5)
                                                   return IVARIABLE ;
                                              if(lookup_word(yytext) == -4)
                                                   return CVARIABLE ;
                                              if(lookup_word(yytext) == -3)
                                                   return FVARIABLE ;
                                              if(lookup_word(yytext) == -2)
                                                   return DVARIABLE ;
                                              if(lookup_word(yytext) == 11)
                                                   return LIVARIABLE ;
                                              if(lookup_word(yytext) == 12)
                                                   return LLIVARIABLE ;
                                          }
<PRINTSTRING>[^\"\n;]+                    {strcpy(yylval.str , yytext) ; return PRSTRING ;}
[,.]                                      { ;}
<PRINTSTRING>"\""                         {BEGIN P ;}
<IDECLARATION>"pointer"                   {strcpy(yylval.str , yytext) ; BEGIN PDECLARATION ; return POINTER ;}
<IDECLARATION>[a-zA-Z][a-zA-Z0-9]*        {add_word(INTDECLARATION , yytext , 0) ; strcpy(yylval.str , yytext) ; return STRING ;}
<IDECLARATION>{DIGIT}+                    {yylval.num = atoll(yytext) ; return INTEGER ;}
<IDECLARATION>"("|")"                     { ;}
<IDECLARATION>"["                         {return yytext[0] ;}
<IDECLARATION>"]"                         {BEGIN INITIAL ; return yytext[0] ;}
<IDECLARATION>"="                         {return yytext[0] ;}
<DDECLARATION>"pointer"                   {strcpy(yylval.str , yytext) ; BEGIN PDECLARATION ; return POINTER ;}
<DDECLARATION>[a-zA-Z][a-zA-Z0-9]*        {add_word(DOUBLEDECLARATION , yytext , 0) ; strcpy(yylval.str , yytext) ; return STRING ;}
<DDECLARATION>{DIGIT}*"."{DIGIT}*         {strcpy(yylval.str , yytext) ; return DOUBLEN ;}
<DDECLARATION>"["                         {return yytext[0] ;}
<DDECLARATION>"]"                         {BEGIN INITIAL ; return yytext[0] ;}
<DDECLARATION>"="                         {return yytext[0] ;}
<INITIAL>"in"                             {strcpy(yylval.str , yytext) ; BEGIN STRUCTVALDEC ; return IN ;}
<PDECLARATION>[a-zA-Z][a-zA-Z0-9]*        {strcpy(yylval.str , yytext) ; add_word(POINTERDECLARATION , yytext , 0) ; BEGIN INITIAL ; return STRING ;}
<STRUCTVALDEC>[a-zA-Z][a-zA-Z0-9]*        {
                                              strcpy(yylval.str , yytext) ;
                                              BEGIN INITIAL ;
                                              if(lookup_word(yytext) == 2)
                                              return PTRSTRING ;
                                              else 
                                              return STRING ;
                                          }
<FDECLARATION>"pointer"                   {strcpy(yylval.str , yytext) ; BEGIN PDECLARATION ; return POINTER ;}
<FDECLARATION>[a-zA-Z][a-zA-Z0-9]*        {add_word(FLOATDECLARATION , yytext , 0) ; strcpy(yylval.str , yytext) ; return STRING ;}
<INITIAL>[a-zA-Z][a-zA-Z0-9]*             {
                                              strcpy(yylval.str , yytext) ; 
                                              if(lookup_word(yytext) == 0){ 
                                                  BEGIN STRUCTSTR ;
                                                  return STRUCTSTRING ; }
                                              else if(lookup_word(yytext) == -2){
                                                  BEGIN DDECLARATION ;
                                                  return STRING ; }
                                              else return STRING ;
                                          }
<STRUCTSTR>"pointer"                      {strcpy(yylval.str , yytext) ; BEGIN STRUCTSTRPTR ; return POINTER ;}
<STRUCTSTRPTR>[a-zA-Z][a-zA-Z0-9]*        {
                                               strcpy(yylval.str , yytext) ; 
                                               add_word(SUBSTRUCTPTRDECLARATION , yytext , 0) ; 
                                               BEGIN INITIAL ; 
                                               return STRING ;
                                          }
<STRUCTSTR>[a-zA-Z][a-zA-Z0-9]*           {
                                               strcpy(yylval.str , yytext) ; 
                                               add_word(SUBSTRUCTDECLARATION , yytext , 0) ; 
                                               BEGIN INITIAL ; 
                                               return STRING ;
                                          }
<INITIAL>"["                              {return yytext[0] ;}
<INITIAL>"]"                              {BEGIN INITIAL ; return yytext[0] ;}
<FDECLARATION>{DIGIT}*"."{DIGIT}*         {strcpy(yylval.str , yytext) ; return FLOATN ;}
<INITIAL>{DIGIT}*"."{DIGIT}*              {strcpy(yylval.str , yytext) ; return FLOATN ;}
<FDECLARATION>"["                         {return yytext[0] ;}
<FDECLARATION>"]"                         {BEGIN INITIAL ; return yytext[0] ;}
<FDECLARATION>"="                         {return yytext[0] ;}
<FDECLARATION>{DIGIT}+                    {yylval.num = atoll(yytext) ; return INTEGER ;}
<CDECLARATION>"\'"[a-zA-Z0-9]"\'"         {strcpy(yylval.str , yytext) ; return CHARN ;}
<INITIAL>"\'"[a-zA-Z0-9]"\'"              {strcpy(yylval.str , yytext) ; return CHARN ;}
<CDECLARATION>"pointer"                   {strcpy(yylval.str , yytext) ; BEGIN PDECLARATION ; return POINTER ;}
<CDECLARATION>[a-zA-Z][a-zA-Z0-9]*        {add_word(CHARDECLARATION , yytext , 0) ; strcpy(yylval.str , yytext) ; return STRING ;}
<CDECLARATION>"["                         {return yytext[0] ;}
<CDECLARATION>"]"                         {BEGIN INITIAL ; return yytext[0] ;}
<CDECLARATION>"="                         {return yytext[0] ;}
<CDECLARATION>{DIGIT}+                    {yylval.num = atoll(yytext) ; return INTEGER ;}
<INITIAL>"="                              {return yytext[0] ;}
<CONDT>[0-9a-zA-Z%<=>!+*/ -]+             {strcpy(yylval.str , yytext) ; BEGIN INITIAL ; return CONDITION ;}
<FORCONDT>"from"                          {strcpy(yylval.str , yytext) ; return STRING ;}
<FORCONDT>"to"                            {strcpy(yylval.str , yytext) ; return STRING ;}
<FORCONDT>"step"                          {strcpy(yylval.str , yytext) ; return STRING ;}
<FORCONDT>[a-zA-Z]+                       {strcpy(yylval.str , yytext) ; return STRING ;}
<FORCONDT>[0-9]+                          {yylval.num = atoll(yytext) ; return INTEGER ;}
<FORCONDT>"{"                             {BEGIN INITIAL ; return yytext[0] ;}
<MINIMUM>[a-zA-Z][a-zA-Z0-9]*             {strcpy(yylval.str , yytext) ; return MSTRING ;}
<MAXIMUM>[a-zA-Z][a-zA-Z0-9]*             {strcpy(yylval.str , yytext) ; return MSTRING ;}
"{"                                       {BEGIN INITIAL ; return yytext[0] ;}
<INITIAL>{DIGIT}+                         {yylval.num = atoll(yytext) ; return INTEGER ;}
<INITIAL>[a-zA-Z]{LETTER}*"."[a-zA-Z]{LETTER}*                                  {strcpy(yylval.str , yytext) ; return STRING ;}
<INITIAL>[a-zA-Z]{LETTER}*"-"">"[a-zA-Z]{LETTER}*                               {strcpy(yylval.str , yytext) ; return STRING ;}
<INITIAL>[a-zA-Z]{LETTER}*"-"">"[a-zA-Z]{LETTER}*"-"">"[a-zA-Z]{LETTER}*        {strcpy(yylval.str , yytext) ; return STRING ;}
<INITIAL>[a-zA-Z0-9+/%*-]+                                                      {strcpy(yylval.str , yytext) ; return EXPR ;}
"}"                                       {return yytext[0] ;}
","|"."|"("|")"|"["|"]"|"{"|"}"           { ;}
"\n"                                      {BEGIN INITIAL;}

%%

struct word{
    char *word_name ;
    int word_type ;
    int word_size ;
    struct word *next ;
};

struct word *word_list ;
extern void *malloc() ;

int add_word(int typ , char *word , int size){
    struct word *wp ;
    wp = (struct word*) malloc(sizeof(struct word)) ;
    wp->next = word_list ;
    wp->word_name = (char*) malloc(strlen(word) + 1) ;
    strcpy(wp->word_name , word) ;
    wp->word_type = typ ;
    wp->word_size = size ;
    word_list = wp ;
    return 1;
}

int lookup_word(char *word){
    struct word *wp = word_list ;
    for( ; wp ; wp = wp->next){
        if(strcmp(wp->word_name , word) == 0){
            if(wp->word_type == ARRAYDECLARATION)
                return wp->word_size ;
            else return wp->word_type ;
        }
    }
    return LOOKUP ;
}

