'''
Este codigo es desarrollado para reconocer la sintaxis del codigo 'CHAPA' usando un analizador
de lexico y sintaxis

conda activate proyecto_lenguajes

--------------ARCHIVOS DE PRUEBA---------------

./test_files/multiplicacionDeMatrices.txt
./test_files/operaciones.txt
./test_files/error.txt
./test_files/error_with_variables.txt
./test_files/operaciones_codigo_intermedio.txt

-----------------------------------------------

Ricardo Abraham Chapa Romero
A00824335
18/04/2021
'''

import sys 
import ply.lex as lex
import ply.yacc as yacc
from queue import Queue, LifoQueue

# Variables globales
variables = [] # arreglo dinamico de variables generadas
variable_type = None # tipo de variables a guardar
token_state = '' # variable to symbolize the token state (Dim, let, ...)
symbol_table = {}

avails = {} # dictionary for available temporary variables {temp var, cuadruplo}
operands = [] # list of operands to perform an operation
'''
TODO:
  Fix the '<=' and '>=' symbol errors in token list
'''
equal_error = False # error handling variable for '<=' and '>=' errors

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

def t_newline(t):
  r'\n+'
  t.lexer.lineno += len(t.value)

def t_error(t):
  t.lexer.skip(1)

# Construimos el lexer
lexer = lex.lex()

''' Funciones para gramatica '''

def p_PROGRAMA(p):
  '''
  PROGRAMA : PROGRAM V M S END
  '''

def p_V(p):
  '''
    V : DIM setType Idv AS T Arr
      | empty
  '''
  global variable_type
  if (len(p) > 2):
    variable_type = p[5].upper()
    add_variables_to_symbol_table(p, variable_type)

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
  p[0] = p[1]

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
  E : LET setType Idv EQUALS Ex
    | DIM setType Idv AS T Arr
    | IF EL THEN F Esf EIF
    | FOR ID EQUALS EA TO Ex F NEXT ID
    | WHILE OPENPAR EL CLOSINGPAR F WEND
    | GOSUB ID
    | INPUT ES COMA IDEx
    | PRINT Ex
  '''
  global variable_type, avails, operands
  if (p[1].upper() == 'DIM'):
    variable_type = p[5].upper()
    add_variables_to_symbol_table(p, variable_type)

  if (p[1].upper() == 'LET'):
    print(str(p[3]) + ' = ' + str(p[5]))

  operands = []

def p_Idv(p):
  '''
  Idv : ID COMA Idv
      | ID
  '''
  global variables
  if (token_state == 'DIM'):
    p.set_lineno(0,p.lineno(1))
    variables.append(p[1])
  
  elif (token_state == 'LET'):
    p[0] = p[1]

def p_setType(p):
  '''
  setType :
  '''
  global token_state
  token_state = p[-1].upper()

def p_Esf(p):
  '''
  Esf : ELSE F
      | empty
  '''

def p_IDEx(p):
  '''
  IDEx : ID
       | ID OPENBRACKET setType Idv CLOSINGBRACKET
  '''

def p_Ex(p):
  '''
  Ex : EA
     | ES
     | EL
     | ID
  '''
  p[0] = p[1]

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
  global operands, avails
  if (len(p) > 3):
    availIndex = str(len(avails))
    operand_1 = operands.pop()
    operand_2 = operands.pop()

    if (p[2] == '+'):
      avails['T' + availIndex] = str('+ ' + str(operand_2) + ' ' + str(operand_1))

    elif (p[2] == '-'):
      avails['T' + availIndex] = str('- ' + str(operand_2) + ' ' + str(operand_1))

    operands.append(str('T' + availIndex))
    p[0] = str('T' + availIndex)

  else:
    p[0] = p[1]
  

def p_P(p):
  '''
  P : N MULTIPLY P
    | N DIVIDE P
    | N
  '''
  global operands, avails
  # skip if value is an assignation
  if (len(p) > 3):
    operand_1 = operands.pop()
    operand_2 = operands.pop()
    availIndex = str(len(avails))

    if (p[2] == '*'):
      avails['T' + availIndex] = str('* ' + str(operand_2) + ' ' + str(operand_1))

    elif (p[2] == '/'):
      avails['T' + availIndex] = str('/ ' + str(operand_2) + ' ' + str(operand_1))

    operands.append(str('T' + availIndex))
    p[0] = str('T' + availIndex)

  else:
    p[0] = p[1]
  

def p_N(p):
  '''
  N : cte saveID
    | ID saveID
    | OPENPAR EA CLOSINGPAR
    | ID OPENBRACKET INTVAL CLOSINGBRACKET
    | ID OPENBRACKET setType Idv CLOSINGBRACKET
  '''
  if (p[1] == '('):
    p[0] = p[2]
  else:
    p[0] = p[1]

def p_saveID(p):
  '''
  saveID :
  '''
  # append id to operands list
  global operands
  operands.append(str(p[-1]))

def p_cte(p):
  '''
  cte : INTVAL
      | FLOATVAL
  '''
  p[0] = p[1]

def p_EL(p):
  '''
  EL : TRUE 
     | FALSE 
     | OPENPAR O CLOSINGPAR
     | OPENPAR O CLOSINGPAR OL EL
  '''
  global operands, avails
  if (p[1] == 'TRUE' or p[1] == 'FALSE'):
    p[0] = p[1]
  else:
    availIndex = str(len(avails))

    if (len(p) > 4):
      avails['T' + availIndex] = str(str(p[4]) + ' ' + str(p[2]) + ' ' + str(p[5]))

      p[0] = str('T' + availIndex)
    else:
      p[0] = p[2]

def p_OL(p):
  '''
  OL : AND
     | OR
     | NOT
  '''
  global operands
  operands = []
  p[0] = p[1]
  
def p_O(p):
  '''
  O : Ex GREATHER Ex
    | Ex GREATHEREQUAL Ex
    | Ex SMALLER Ex
    | Ex SMALLEREQUAL Ex
    | Ex NOTEQUAL Ex
    | Ex EQUALTO Ex
  '''
  global operands, avails, equal_error
  
  operand_1 = p[1]
  operand_2 = p[3]

  operation = str(p[2])

  if (equal_error):
    operation += '='
    equal_error = False

  if (len(operands) == 2):
    operand_2 = operands.pop()
    operand_1 = operands.pop()
  elif (len(operands) == 1):
    operand_1 = operands.pop()

  availIndex = str(len(avails))
  avails['T' + availIndex] = str(operation + ' ' + str(operand_1) + ' ' + str(operand_2))
  operands.append(str('T' + availIndex))

  p[0] = str('T' + availIndex)

def p_O_error(p):
  '''
  O : Ex error Ex
  '''
  print('Error in boolean operand')
  #print(p)

def p_empty(p):
  '''
  empty :
  '''
  p[0] = None
  pass

def p_error(p):
  if (p.type == 'EQUALS'):
    global equal_error
    equal_error = True
    parser.errok()
  else:
    print('\tSintaxis Incorrecto\n')
    print('Error: ' + str(p))

def add_variables_to_symbol_table(p, variable_type):
  '''
  Function to add variables id and type to the 
  symbol table.

  @variable_type (string): variable type to be inserted
                           in the symbol table


  Variable types to int:
    INT   = 0
    FLOAT = 1
    WORD  = 2
  '''
  variable_type_to_int = {'INT': 0, 'FLOAT': 1, 'WORD': 2}
  
  global variables, symbol_table

  for variable in variables:
    if (symbol_table.get(variable, -1) == -1):
      symbol_table[variable] = (variable_type_to_int[variable_type], p.lineno(1))
    else:
      print('ERROR: The variable \'' + variable + '\' is already defined in line: ' + str(symbol_table[variable][1]))
      print('Variable redefined at line: ' + str(p.lineno(1)) + '\n')

  variables = []

parser = yacc.yacc() # creamos el parser para analisis de gramatica

def print_symbol_table(symbol_table):
  variable_int_to_type = ['INT','FLOAT','WORD']
  i = 0
  print('| Index | Variable | Variable Type | Line |\n')
  for key in symbol_table:
    print('| ', i, ' | ', key, ' | ', variable_int_to_type[symbol_table[key][0]],' | ', symbol_table[key][1], '|\n')
    i+=1


def print_avails_table(avails):
  print('| Avail | Cuadruplo |\n')
  for avail in avails:
    print('| ', avail, ' | ', avails[avail], ' |\n')

def print_syntax_info_tables():
  global symbol_table, avails

  print('\nSymbol table')
  print_symbol_table(symbol_table)

  print('\nAvail/Cuadruplos table')
  print_avails_table(avails)


try:
  fileDirectory = input('Directorio al archivo de prueba: ')
  print('')
  f = open(fileDirectory, 'r')
  testFile = f.read()
  parser.parse(testFile, tracking=True)

  print_syntax_info_tables()
except EOFError:
  print('Error at reading the file')
  pass
