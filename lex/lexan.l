%{
int yywrap();
int myf();

int yylval;
%}

delim    [ \t\n]
ws       {delim}+
letter   [A-Za-z_]
digit    [0-9]
id       {letter}({letter}|{digit})+
number   {digit}+
specials      [\{\}\(\);\<\>\+\-\*\/]

%%

{ws}      { /* no action and no return */ }

{specials}     {return myf();}
{id}     {return myf();}
{number}     {return myf();}
":="     {return myf();}
"."     {return myf();}

%%

int myf(){
    printf("'%s', ", yytext);
    return 1;
}

int yywrap(){return 1;}
