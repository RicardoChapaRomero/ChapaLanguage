'''
Este codigo es desarrollado para reconocer la sintaxis del codigo 'CHAPA' usando un analizador
de lexico y sintaxis

conda activate proyecto_lenguajes

Ricardo Abraham Chapa Romero
A00824335
10/04/2021
'''

import sys 
import ply.lex as lex
import ply.yacc as yacc

# Lista de tokens a utilizar
tokens = [
	'PROGRAM', # tokens del programa
  'END',
  'AND', # operadores logicos
  'OR',
  'NOT',
  'TRUE',
  'FALSE',
  'EQUALTO', # operadores relacionales
  'GREATHER',
  'GREATHEREQUAL',
  'SMALLER',
  'SMALLEREQUAL',
  'NOTEQUAL',
  'PLUS', # operadores aritmeticos
  'MINUS',
  'MULTIPLY',
  'DIVIDE',
  'IF', # estatuos condicionales - if
  'THEN',
  'ELSE',
  'EIF',
  'WHILE', # ciclos - while
  'WEND',
  'FOR', # ciclos - for
  'TO',
  'NEXT',
  'SUBPROCEDURE', # <- procedimientos o funciones
  'RETURN',
  'GOSUB', # operaciones del programa
  'INPUT',
  'PRINT',
  'DIM', # declaración de variables
  'AS',
  'LET',
  'EQUALS', # asignación y tipos de datos
  'INT',
  'INTVAL',
  'FLOATVAL',
  'FLOAT',
  'WORD',
  'WORDVAL',
  'ID',
  'COMA', # symbols
  'TWOPOINTS',
  'OPENPAR',
  'CLOSINGPAR',
  'OPENBRACKET',
  'CLOSINGBRACKET',
  'COMMENT'
]

t_PLUS = r'\+'
t_MINUS = r'\-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'\/'
t_EQUALS = r'\='

#t_ignore = r' ' # ignorar espacios

t_ignore_COMMENT = r'\#.*'

''' Funciones para analisis de lexico en ER '''
def t_PROGRAM(t):
  r'(?i)PROGRAM'
  t.type = 'PROGRAM'
  return t

def t_END(t):
  r'(?i)END'
  t.type = 'END'
  return t

def t_AND(t):
  r'(?i)AND'
  t.type = 'AND'
  return t

def t_OR(t):
  r'(?i)OR'
  t.type = 'OR'
  return t

def t_NOT(t):
  r'(?i)NOT'
  t.type = 'NOT'
  return t

def t_TRUE(t):
  r'(?i)TRUE'
  t.type = 'TRUE'
  return t

def t_FALSE(t):
  r'(?i)FALSE'
  t.type = 'FALSE'
  return t

def t_IF(t):
  r'(?i)IF'
  t.type = 'IF'
  return t

def t_THEN(t):
  r'(?i)THEN'
  t.type = 'THEN'
  return t

def t_ELSE(t):
  r'(?i)ELSE'
  t.type = 'ELSE'
  return t

def t_EIF(t):
  r'(?i)EIF'
  t.type = 'EIF'
  return t

def t_WHILE(t):
  r'(?i)WHILE'
  t.type = 'WHILE'
  return t

def t_WEND(t):
  r'(?i)WEND'
  t.type = 'WEND'
  return t

def t_FOR(t):
  r'(?i)FOR'
  t.type = 'FOR'
  return t

def t_TO(t):
  r'(?i)TO'
  t.type = 'TO'
  return t

def t_NEXT(t):
  r'(?i)NEXT'
  t.type = 'NEXT'
  return t

def t_SUBPROCEDURE(t):
  r'(?i)SUBPROCEDURE'
  t.type = 'SUBPROCEDURE'
  return t

def t_RETURN(t):
  r'(?i)RETURN'
  t.type = 'RETURN'
  return t

def t_GOSUB(t):
  r'(?i)GOSUB'
  t.type = 'GOSUB'
  return t

def t_INPUT(t):
  r'(?i)INPUT'
  t.type = 'INPUT'
  return t

def t_PRINT(t):
  r'(?i)PRINT'
  t.type = 'PRINT'
  return t

def t_DIM(t):
  r'(?i)DIM'
  t.type = 'DIM'
  return t

def t_AS(t):
  r'(?i)AS'
  t.type = 'AS'
  return t

def t_LET(t):
  r'(?i)LET'
  t.type = 'LET'
  return t

def t_INT(t):
  r'(?i)INT'
  t.type = 'INT'
  return t

def t_INTVAL(t):
  r'\d+'
  t.value = int(t.value)
  return t

def t_FLOAT(t):
  r'(?i)FLOAT'
  t.type = 'FLOAT'
  return t

def t_FLOATVAL(t):
  r'\d+\.\d+'
  t.value = float(t.value)
  return t

def t_WORD(t):
  r'(?i)WORD'
  t.type = 'WORD'
  return t

def t_WORDVAL(t):
  r'\".*\"'
  t.type = 'WORDVAL'
  return t

def t_ID(t):
  r'[a-zA-Z_][a-zA-Z_0-9]*'
  t.type = 'ID'
  return t

def t_EQUALTO(t):
  r'\=\='
  t.type = 'EQUALTO'
  return t

def t_GREATHER(t):
  r'\>'
  t.type = 'GREATHER'
  return t

def t_GREATHEREQUAL(t):
  r'\>\='
  t.type = 'GREATHEREQUAL'
  return t

def t_SMALLER(t):
  r'\<'
  t.type = 'SMALLER'
  return t

def t_SMALLEREQUAL(t):
  r'\<\='
  t.type = 'SMALLEREQUAL'
  return t

def t_NOTEQUAL(t):
  r'\!\='
  t.type = 'NOTEQUAL'
  return t

def t_COMA(t):
	r'\,'
	t.type = 'COMA'
	return t

def t_TWOPOINTS(t):
  r'\:'
  t.type = 'TWOPOINTS'
  return t

def t_OPENPAR(t):
  r'\('
  t.type = 'OPENPAR'
  return t

def t_CLOSINGPAR(t):
  r'\)'
  t.type = 'CLOSINGPAR'
  return t

def t_OPENBRACKET(t):
  r'\['
  t.type = 'OPENBRACKET'
  return t

def t_CLOSINGBRACKET(t):
  r'\]'
  t.type = 'CLOSINGBRACKET'
  return t

def t_error(t):
  t.lexer.skip(1)

# Construimos el lexer
lexer = lex.lex()

''' Funciones para gramatica '''

def p_PROGRAMA(p):
  '''
  PROGRAMA : PROGRAM V M S END
  '''
  print("\tSintaxis correcto\n")

def p_V(p):
  '''
    V : DIM Idv AS T Arr V
      | empty
  '''

def p_Idv(p):
  '''
  Idv : ID COMA Idv
      | ID
  '''

def p_Arr(p):
  '''
  Arr : OPENBRACKET EA CLOSINGBRACKET Arr
      | OPENBRACKET ID CLOSINGBRACKET Arr
      | empty
  '''

def p_T(p):
  '''
  T : INT
    | FLOAT
    | WORD
  '''

def p_S(p):
  '''
  S : SUBPROCEDURE ID TWOPOINTS M RETURN S
    | empty
  '''

def p_M(p):
  '''
  M : F M
    | empty
  '''

def p_F(p):
  '''
  F : E F
    | empty
  '''

def p_E(p):
  '''
  E : LET Idv EQUALS Ex
    | DIM Idv AS T Arr 
    | IF EL THEN F Esf EIF
    | FOR ID EQUALS EA TO Ex F NEXT ID
    | WHILE OPENPAR EL CLOSINGPAR F WEND
    | GOSUB ID
    | INPUT ES COMA IDEx
    | PRINT Ex
  '''

def p_Esf(p):
  '''
  Esf : ELSE F
      | empty
  '''

def p_IDEx(p):
  '''
  IDEx : ID
       | ID OPENBRACKET Idv CLOSINGBRACKET
  '''

def p_Ex(p):
  '''
  Ex : EA
     | ES
     | EL
     | ID
  '''

def p_ES(p):
  '''
  ES : WORDVAL
  '''

def p_EA(p):
  '''
  EA : P PLUS EA
     | P MINUS EA
     | P 
  '''

def p_P(p):
  '''
  P : N MULTIPLY P
    | N DIVIDE P
    | N
  '''

def p_N(p):
  '''
  N : cte
    | ID
    | OPENPAR EA CLOSINGPAR
    | ID OPENBRACKET INTVAL CLOSINGBRACKET
    | ID OPENBRACKET Idv CLOSINGBRACKET
  '''

def p_cte(p):
  '''
  cte : INTVAL
      | FLOATVAL
  '''

def p_EL(p):
  '''
  EL : TRUE 
     | FALSE 
     | OPENPAR O CLOSINGPAR Olt
  '''

def p_Olt(p):
  '''
  Olt : OL OPENPAR O CLOSINGPAR Olt
      | empty
  '''

def p_OL(p):
  '''
  OL : AND
     | OR
     | NOT
  '''

def p_O(p):
  '''
  O : WORD EQUALTO WORD
    | EA OPR EA
    | ID OPR ID
    | ID OPR EA
    | EA OPR ID
  '''

def p_OPR(p):
  '''
  OPR : EQUALTO
      | GREATHER
      | GREATHEREQUAL
      | SMALLER
      | SMALLEREQUAL
      | NOTEQUAL
  '''

def p_empty(p):
  '''
  empty :
  '''
  pass

def p_error(p):
  print('\tSintaxis Incorrecto\n')

parser = yacc.yacc() # creamos el parser para analisis de gramatica

try:
  print('PRUEBA DE MULTIPLICACION DE MATRICES')
  f = open('./test_files/multiplicacionDeMatrices.txt', 'r')
  testFile = f.read()
  parser.parse(testFile)
except EOFError:
  print('Error at reading the file')
  pass

try:
  print('PRUEBA DE OPERACIONES ANIDADAS')
  f = open('./test_files/operaciones.txt', 'r')
  testFile = f.read()
  parser.parse(testFile)
except EOFError:
  print('Error at reading the file')
  pass

try:
  print('PRUEBA DE CODIGO CON ERROR')
  f = open('./test_files/error.txt', 'r')
  testFile = f.read()
  parser.parse(testFile)
except EOFError:
  print('Error at reading the file')
  pass
