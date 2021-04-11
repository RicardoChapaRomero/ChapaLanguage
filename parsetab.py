
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'AND AS CLOSINGBRACKET CLOSINGPAR COMA COMMENT DIM DIVIDE EIF ELSE END EQUALS EQUALTO FALSE FLOAT FLOATVAL FOR GOSUB GREATHER GREATHEREQUAL ID IF INPUT INT INTVAL LET MINUS MULTIPLY NEXT NOT NOTEQUAL OPENBRACKET OPENPAR OR PLUS PRINT PROGRAM RETURN SMALLER SMALLEREQUAL SUBPROCEDURE THEN TO TRUE TWOPOINTS WEND WHILE WORD WORDVAL\n  PROGRAMA : PROGRAM V M S END\n  \n    V : DIM Idv AS T Arr V\n      | empty\n  \n  Idv : ID COMA Idv\n      | ID\n  \n  Arr : OPENBRACKET EA CLOSINGBRACKET Arr\n      | OPENBRACKET ID CLOSINGBRACKET Arr\n      | empty\n  \n  T : INT\n    | FLOAT\n    | WORD\n  \n  S : SUBPROCEDURE ID TWOPOINTS M RETURN S\n    | empty\n  \n  M : F M\n    | empty\n  \n  F : E F\n    | empty\n  \n  E : LET Idv EQUALS Ex\n    | DIM Idv AS T Arr \n    | IF EL THEN F Esf EIF\n    | FOR ID EQUALS EA TO Ex F NEXT ID\n    | WHILE OPENPAR EL CLOSINGPAR F WEND\n    | GOSUB ID\n    | INPUT ES COMA IDEx\n    | PRINT Ex\n  \n  Esf : ELSE F\n      | empty\n  \n  IDEx : ID\n       | ID OPENBRACKET Idv CLOSINGBRACKET\n  \n  Ex : EA\n     | ES\n     | EL\n     | ID\n  \n  ES : WORDVAL\n  \n  EA : P PLUS EA\n     | P MINUS EA\n     | P \n  \n  P : N MULTIPLY P\n    | N DIVIDE P\n    | N\n  \n  N : cte\n    | ID\n    | OPENPAR EA CLOSINGPAR\n    | ID OPENBRACKET INTVAL CLOSINGBRACKET\n    | ID OPENBRACKET Idv CLOSINGBRACKET\n  \n  cte : INTVAL\n      | FLOATVAL\n  \n  EL : TRUE \n     | FALSE \n     | OPENPAR O CLOSINGPAR Olt\n  \n  Olt : OL OPENPAR O CLOSINGPAR Olt\n      | empty\n  \n  OL : AND\n     | OR\n     | NOT\n  \n  O : WORD EQUALTO WORD\n    | EA OPR EA\n    | ID OPR ID\n    | ID OPR EA\n    | EA OPR ID\n  \n  OPR : EQUALTO\n      | GREATHER\n      | GREATHEREQUAL\n      | SMALLER\n      | SMALLEREQUAL\n      | NOTEQUAL\n  \n  empty :\n  '
    
_lr_action_items = {'PROGRAM':([0,],[2,]),'$end':([1,50,],[0,-1,]),'DIM':([2,3,5,7,8,9,24,25,29,30,34,36,37,38,39,40,41,42,44,45,46,47,54,69,70,71,72,74,75,76,79,80,91,92,93,96,97,98,99,100,101,103,105,107,109,111,123,124,125,129,132,133,135,136,140,141,142,143,145,146,],[4,11,-3,11,-17,11,-16,-17,-48,-49,-23,-34,-25,-30,-31,-32,-33,-37,-40,-41,-46,-47,11,-67,-9,-10,-11,11,-18,-67,-42,-67,11,-24,-28,-35,-36,-43,-38,-39,4,-8,-19,11,-50,-52,-44,-45,-2,-20,11,-22,-67,-67,-29,-6,-7,-67,-51,-21,]),'LET':([2,3,5,7,8,9,24,25,29,30,34,36,37,38,39,40,41,42,44,45,46,47,54,69,70,71,72,74,75,76,79,80,91,92,93,96,97,98,99,100,101,103,105,107,109,111,123,124,125,129,132,133,135,136,140,141,142,143,145,146,],[-67,10,-3,10,-17,10,-16,-17,-48,-49,-23,-34,-25,-30,-31,-32,-33,-37,-40,-41,-46,-47,10,-67,-9,-10,-11,10,-18,-67,-42,-67,10,-24,-28,-35,-36,-43,-38,-39,-67,-8,-19,10,-50,-52,-44,-45,-2,-20,10,-22,-67,-67,-29,-6,-7,-67,-51,-21,]),'IF':([2,3,5,7,8,9,24,25,29,30,34,36,37,38,39,40,41,42,44,45,46,47,54,69,70,71,72,74,75,76,79,80,91,92,93,96,97,98,99,100,101,103,105,107,109,111,123,124,125,129,132,133,135,136,140,141,142,143,145,146,],[-67,12,-3,12,-17,12,-16,-17,-48,-49,-23,-34,-25,-30,-31,-32,-33,-37,-40,-41,-46,-47,12,-67,-9,-10,-11,12,-18,-67,-42,-67,12,-24,-28,-35,-36,-43,-38,-39,-67,-8,-19,12,-50,-52,-44,-45,-2,-20,12,-22,-67,-67,-29,-6,-7,-67,-51,-21,]),'FOR':([2,3,5,7,8,9,24,25,29,30,34,36,37,38,39,40,41,42,44,45,46,47,54,69,70,71,72,74,75,76,79,80,91,92,93,96,97,98,99,100,101,103,105,107,109,111,123,124,125,129,132,133,135,136,140,141,142,143,145,146,],[-67,13,-3,13,-17,13,-16,-17,-48,-49,-23,-34,-25,-30,-31,-32,-33,-37,-40,-41,-46,-47,13,-67,-9,-10,-11,13,-18,-67,-42,-67,13,-24,-28,-35,-36,-43,-38,-39,-67,-8,-19,13,-50,-52,-44,-45,-2,-20,13,-22,-67,-67,-29,-6,-7,-67,-51,-21,]),'WHILE':([2,3,5,7,8,9,24,25,29,30,34,36,37,38,39,40,41,42,44,45,46,47,54,69,70,71,72,74,75,76,79,80,91,92,93,96,97,98,99,100,101,103,105,107,109,111,123,124,125,129,132,133,135,136,140,141,142,143,145,146,],[-67,14,-3,14,-17,14,-16,-17,-48,-49,-23,-34,-25,-30,-31,-32,-33,-37,-40,-41,-46,-47,14,-67,-9,-10,-11,14,-18,-67,-42,-67,14,-24,-28,-35,-36,-43,-38,-39,-67,-8,-19,14,-50,-52,-44,-45,-2,-20,14,-22,-67,-67,-29,-6,-7,-67,-51,-21,]),'GOSUB':([2,3,5,7,8,9,24,25,29,30,34,36,37,38,39,40,41,42,44,45,46,47,54,69,70,71,72,74,75,76,79,80,91,92,93,96,97,98,99,100,101,103,105,107,109,111,123,124,125,129,132,133,135,136,140,141,142,143,145,146,],[-67,15,-3,15,-17,15,-16,-17,-48,-49,-23,-34,-25,-30,-31,-32,-33,-37,-40,-41,-46,-47,15,-67,-9,-10,-11,15,-18,-67,-42,-67,15,-24,-28,-35,-36,-43,-38,-39,-67,-8,-19,15,-50,-52,-44,-45,-2,-20,15,-22,-67,-67,-29,-6,-7,-67,-51,-21,]),'INPUT':([2,3,5,7,8,9,24,25,29,30,34,36,37,38,39,40,41,42,44,45,46,47,54,69,70,71,72,74,75,76,79,80,91,92,93,96,97,98,99,100,101,103,105,107,109,111,123,124,125,129,132,133,135,136,140,141,142,143,145,146,],[-67,16,-3,16,-17,16,-16,-17,-48,-49,-23,-34,-25,-30,-31,-32,-33,-37,-40,-41,-46,-47,16,-67,-9,-10,-11,16,-18,-67,-42,-67,16,-24,-28,-35,-36,-43,-38,-39,-67,-8,-19,16,-50,-52,-44,-45,-2,-20,16,-22,-67,-67,-29,-6,-7,-67,-51,-21,]),'PRINT':([2,3,5,7,8,9,24,25,29,30,34,36,37,38,39,40,41,42,44,45,46,47,54,69,70,71,72,74,75,76,79,80,91,92,93,96,97,98,99,100,101,103,105,107,109,111,123,124,125,129,132,133,135,136,140,141,142,143,145,146,],[-67,17,-3,17,-17,17,-16,-17,-48,-49,-23,-34,-25,-30,-31,-32,-33,-37,-40,-41,-46,-47,17,-67,-9,-10,-11,17,-18,-67,-42,-67,17,-24,-28,-35,-36,-43,-38,-39,-67,-8,-19,17,-50,-52,-44,-45,-2,-20,17,-22,-67,-67,-29,-6,-7,-67,-51,-21,]),'SUBPROCEDURE':([2,3,5,6,7,8,9,23,24,25,29,30,34,36,37,38,39,40,41,42,44,45,46,47,69,70,71,72,75,76,79,80,92,93,96,97,98,99,100,101,103,105,109,111,123,124,125,128,129,133,135,136,140,141,142,143,145,146,],[-67,-67,-3,21,-67,-15,-67,-14,-16,-17,-48,-49,-23,-34,-25,-30,-31,-32,-33,-37,-40,-41,-46,-47,-67,-9,-10,-11,-18,-67,-42,-67,-24,-28,-35,-36,-43,-38,-39,-67,-8,-19,-50,-52,-44,-45,-2,21,-20,-22,-67,-67,-29,-6,-7,-67,-51,-21,]),'END':([2,3,5,6,7,8,9,20,22,23,24,25,29,30,34,36,37,38,39,40,41,42,44,45,46,47,69,70,71,72,75,76,79,80,92,93,96,97,98,99,100,101,103,105,109,111,123,124,125,128,129,133,135,136,137,140,141,142,143,145,146,],[-67,-67,-3,-67,-67,-15,-67,50,-13,-14,-16,-17,-48,-49,-23,-34,-25,-30,-31,-32,-33,-37,-40,-41,-46,-47,-67,-9,-10,-11,-18,-67,-42,-67,-24,-28,-35,-36,-43,-38,-39,-67,-8,-19,-50,-52,-44,-45,-2,-67,-20,-22,-67,-67,-12,-29,-6,-7,-67,-51,-21,]),'ID':([4,10,11,13,15,17,21,31,43,49,52,55,60,62,63,64,65,67,68,82,83,84,85,86,87,88,89,102,120,122,131,144,],[19,19,19,32,34,41,51,59,59,19,41,79,79,93,19,79,79,79,79,117,-61,-62,-63,-64,-65,-66,118,127,41,19,59,146,]),'RETURN':([7,8,9,23,24,25,29,30,34,36,37,38,39,40,41,42,44,45,46,47,70,71,72,74,75,76,79,80,92,93,96,97,98,99,100,103,104,105,109,111,123,124,129,133,135,136,140,141,142,143,145,146,],[-67,-15,-67,-14,-16,-17,-48,-49,-23,-34,-25,-30,-31,-32,-33,-37,-40,-41,-46,-47,-9,-10,-11,-67,-18,-67,-42,-67,-24,-28,-35,-36,-43,-38,-39,-8,128,-19,-50,-52,-44,-45,-20,-22,-67,-67,-29,-6,-7,-67,-51,-21,]),'ELSE':([9,24,25,29,30,34,36,37,38,39,40,41,42,44,45,46,47,54,70,71,72,75,76,77,79,80,92,93,96,97,98,99,100,103,105,109,111,123,124,129,133,135,136,140,141,142,143,145,146,],[-67,-16,-17,-48,-49,-23,-34,-25,-30,-31,-32,-33,-37,-40,-41,-46,-47,-67,-9,-10,-11,-18,-67,107,-42,-67,-24,-28,-35,-36,-43,-38,-39,-8,-19,-50,-52,-44,-45,-20,-22,-67,-67,-29,-6,-7,-67,-51,-21,]),'EIF':([9,24,25,29,30,34,36,37,38,39,40,41,42,44,45,46,47,54,70,71,72,75,76,77,79,80,92,93,96,97,98,99,100,103,105,106,107,108,109,111,123,124,129,130,133,135,136,140,141,142,143,145,146,],[-67,-16,-17,-48,-49,-23,-34,-25,-30,-31,-32,-33,-37,-40,-41,-46,-47,-67,-9,-10,-11,-18,-67,-67,-42,-67,-24,-28,-35,-36,-43,-38,-39,-8,-19,129,-67,-27,-50,-52,-44,-45,-20,-26,-22,-67,-67,-29,-6,-7,-67,-51,-21,]),'WEND':([9,24,25,29,30,34,36,37,38,39,40,41,42,44,45,46,47,70,71,72,75,76,79,80,91,92,93,96,97,98,99,100,103,105,109,111,121,123,124,129,133,135,136,140,141,142,143,145,146,],[-67,-16,-17,-48,-49,-23,-34,-25,-30,-31,-32,-33,-37,-40,-41,-46,-47,-9,-10,-11,-18,-67,-42,-67,-67,-24,-28,-35,-36,-43,-38,-39,-8,-19,-50,-52,133,-44,-45,-20,-22,-67,-67,-29,-6,-7,-67,-51,-21,]),'NEXT':([9,24,25,29,30,34,36,37,38,39,40,41,42,44,45,46,47,70,71,72,75,76,79,80,92,93,96,97,98,99,100,103,105,109,111,123,124,129,132,133,135,136,139,140,141,142,143,145,146,],[-67,-16,-17,-48,-49,-23,-34,-25,-30,-31,-32,-33,-37,-40,-41,-46,-47,-9,-10,-11,-18,-67,-42,-67,-24,-28,-35,-36,-43,-38,-39,-8,-19,-50,-52,-44,-45,-20,-67,-22,-67,-67,144,-29,-6,-7,-67,-51,-21,]),'TRUE':([12,17,33,52,120,],[29,29,29,29,29,]),'FALSE':([12,17,33,52,120,],[30,30,30,30,30,]),'OPENPAR':([12,14,17,31,33,43,52,55,60,64,65,67,68,82,83,84,85,86,87,88,89,102,110,112,113,114,120,131,],[31,33,43,55,31,55,43,55,55,55,55,55,55,55,-61,-62,-63,-64,-65,-66,55,55,131,-53,-54,-55,43,55,]),'WORDVAL':([16,17,52,120,],[36,36,36,36,]),'INTVAL':([17,31,43,52,55,60,63,64,65,67,68,82,83,84,85,86,87,88,89,102,120,131,],[46,46,46,46,46,46,94,46,46,46,46,46,-61,-62,-63,-64,-65,-66,46,46,46,46,]),'FLOATVAL':([17,31,43,52,55,60,64,65,67,68,82,83,84,85,86,87,88,89,102,120,131,],[47,47,47,47,47,47,47,47,47,47,47,-61,-62,-63,-64,-65,-66,47,47,47,47,]),'AS':([18,19,27,73,],[48,-5,53,-4,]),'COMA':([19,35,36,],[49,62,-34,]),'EQUALS':([19,26,32,73,],[-5,52,60,-4,]),'CLOSINGBRACKET':([19,42,44,45,46,47,73,79,94,95,96,97,98,99,100,123,124,126,127,134,],[-5,-37,-40,-41,-46,-47,-4,-42,123,124,-35,-36,-43,-38,-39,-44,-45,135,136,140,]),'THEN':([28,29,30,80,109,111,143,145,],[54,-48,-49,-67,-50,-52,-67,-51,]),'CLOSINGPAR':([29,30,42,44,45,46,47,56,59,61,66,78,79,80,96,97,98,99,100,109,111,115,116,117,118,119,123,124,138,143,145,],[-48,-49,-37,-40,-41,-46,-47,80,-42,91,98,98,-42,-67,-35,-36,-43,-38,-39,-50,-52,-56,-57,-42,-42,-59,-44,-45,143,-67,-51,]),'WORD':([31,43,48,53,81,131,],[57,57,72,72,115,57,]),'MULTIPLY':([41,44,45,46,47,59,79,98,117,118,123,124,127,],[-42,67,-41,-46,-47,-42,-42,-43,-42,-42,-44,-45,-42,]),'DIVIDE':([41,44,45,46,47,59,79,98,117,118,123,124,127,],[-42,68,-41,-46,-47,-42,-42,-43,-42,-42,-44,-45,-42,]),'PLUS':([41,42,44,45,46,47,59,79,98,99,100,117,118,123,124,127,],[-42,64,-40,-41,-46,-47,-42,-42,-43,-38,-39,-42,-42,-44,-45,-42,]),'MINUS':([41,42,44,45,46,47,59,79,98,99,100,117,118,123,124,127,],[-42,65,-40,-41,-46,-47,-42,-42,-43,-38,-39,-42,-42,-44,-45,-42,]),'OPENBRACKET':([41,59,69,70,71,72,76,79,93,117,118,127,135,136,],[63,63,102,-9,-10,-11,102,63,122,63,63,63,102,102,]),'EQUALTO':([42,44,45,46,47,57,58,59,66,79,96,97,98,99,100,123,124,],[-37,-40,-41,-46,-47,81,83,83,83,-42,-35,-36,-43,-38,-39,-44,-45,]),'GREATHER':([42,44,45,46,47,58,59,66,79,96,97,98,99,100,123,124,],[-37,-40,-41,-46,-47,84,84,84,-42,-35,-36,-43,-38,-39,-44,-45,]),'GREATHEREQUAL':([42,44,45,46,47,58,59,66,79,96,97,98,99,100,123,124,],[-37,-40,-41,-46,-47,85,85,85,-42,-35,-36,-43,-38,-39,-44,-45,]),'SMALLER':([42,44,45,46,47,58,59,66,79,96,97,98,99,100,123,124,],[-37,-40,-41,-46,-47,86,86,86,-42,-35,-36,-43,-38,-39,-44,-45,]),'SMALLEREQUAL':([42,44,45,46,47,58,59,66,79,96,97,98,99,100,123,124,],[-37,-40,-41,-46,-47,87,87,87,-42,-35,-36,-43,-38,-39,-44,-45,]),'NOTEQUAL':([42,44,45,46,47,58,59,66,79,96,97,98,99,100,123,124,],[-37,-40,-41,-46,-47,88,88,88,-42,-35,-36,-43,-38,-39,-44,-45,]),'TO':([42,44,45,46,47,79,90,96,97,98,99,100,123,124,],[-37,-40,-41,-46,-47,-42,120,-35,-36,-43,-38,-39,-44,-45,]),'INT':([48,53,],[70,70,]),'FLOAT':([48,53,],[71,71,]),'TWOPOINTS':([51,],[74,]),'AND':([80,143,],[112,112,]),'OR':([80,143,],[113,113,]),'NOT':([80,143,],[114,114,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'PROGRAMA':([0,],[1,]),'V':([2,101,],[3,125,]),'empty':([2,3,6,7,9,54,69,74,76,77,80,91,101,107,128,132,135,136,143,],[5,8,22,8,25,25,103,8,103,108,111,25,5,25,22,25,103,103,111,]),'M':([3,7,74,],[6,23,104,]),'F':([3,7,9,54,74,91,107,132,],[7,7,24,77,7,121,130,139,]),'E':([3,7,9,54,74,91,107,132,],[9,9,9,9,9,9,9,9,]),'Idv':([4,10,11,49,63,122,],[18,26,27,73,95,134,]),'S':([6,128,],[20,137,]),'EL':([12,17,33,52,120,],[28,40,61,40,40,]),'ES':([16,17,52,120,],[35,39,39,39,]),'Ex':([17,52,120,],[37,75,132,]),'EA':([17,31,43,52,55,60,64,65,82,89,102,120,131,],[38,58,66,38,78,90,96,97,116,119,126,38,58,]),'P':([17,31,43,52,55,60,64,65,67,68,82,89,102,120,131,],[42,42,42,42,42,42,42,42,99,100,42,42,42,42,42,]),'N':([17,31,43,52,55,60,64,65,67,68,82,89,102,120,131,],[44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,]),'cte':([17,31,43,52,55,60,64,65,67,68,82,89,102,120,131,],[45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,]),'O':([31,43,131,],[56,56,138,]),'T':([48,53,],[69,76,]),'OPR':([58,59,66,],[82,89,82,]),'IDEx':([62,],[92,]),'Arr':([69,76,135,136,],[101,105,141,142,]),'Esf':([77,],[106,]),'Olt':([80,143,],[109,145,]),'OL':([80,143,],[110,110,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> PROGRAMA","S'",1,None,None,None),
  ('PROGRAMA -> PROGRAM V M S END','PROGRAMA',5,'p_PROGRAMA','syntax_analyzer.py',305),
  ('V -> DIM Idv AS T Arr V','V',6,'p_V','syntax_analyzer.py',311),
  ('V -> empty','V',1,'p_V','syntax_analyzer.py',312),
  ('Idv -> ID COMA Idv','Idv',3,'p_Idv','syntax_analyzer.py',317),
  ('Idv -> ID','Idv',1,'p_Idv','syntax_analyzer.py',318),
  ('Arr -> OPENBRACKET EA CLOSINGBRACKET Arr','Arr',4,'p_Arr','syntax_analyzer.py',323),
  ('Arr -> OPENBRACKET ID CLOSINGBRACKET Arr','Arr',4,'p_Arr','syntax_analyzer.py',324),
  ('Arr -> empty','Arr',1,'p_Arr','syntax_analyzer.py',325),
  ('T -> INT','T',1,'p_T','syntax_analyzer.py',330),
  ('T -> FLOAT','T',1,'p_T','syntax_analyzer.py',331),
  ('T -> WORD','T',1,'p_T','syntax_analyzer.py',332),
  ('S -> SUBPROCEDURE ID TWOPOINTS M RETURN S','S',6,'p_S','syntax_analyzer.py',337),
  ('S -> empty','S',1,'p_S','syntax_analyzer.py',338),
  ('M -> F M','M',2,'p_M','syntax_analyzer.py',343),
  ('M -> empty','M',1,'p_M','syntax_analyzer.py',344),
  ('F -> E F','F',2,'p_F','syntax_analyzer.py',349),
  ('F -> empty','F',1,'p_F','syntax_analyzer.py',350),
  ('E -> LET Idv EQUALS Ex','E',4,'p_E','syntax_analyzer.py',355),
  ('E -> DIM Idv AS T Arr','E',5,'p_E','syntax_analyzer.py',356),
  ('E -> IF EL THEN F Esf EIF','E',6,'p_E','syntax_analyzer.py',357),
  ('E -> FOR ID EQUALS EA TO Ex F NEXT ID','E',9,'p_E','syntax_analyzer.py',358),
  ('E -> WHILE OPENPAR EL CLOSINGPAR F WEND','E',6,'p_E','syntax_analyzer.py',359),
  ('E -> GOSUB ID','E',2,'p_E','syntax_analyzer.py',360),
  ('E -> INPUT ES COMA IDEx','E',4,'p_E','syntax_analyzer.py',361),
  ('E -> PRINT Ex','E',2,'p_E','syntax_analyzer.py',362),
  ('Esf -> ELSE F','Esf',2,'p_Esf','syntax_analyzer.py',367),
  ('Esf -> empty','Esf',1,'p_Esf','syntax_analyzer.py',368),
  ('IDEx -> ID','IDEx',1,'p_IDEx','syntax_analyzer.py',373),
  ('IDEx -> ID OPENBRACKET Idv CLOSINGBRACKET','IDEx',4,'p_IDEx','syntax_analyzer.py',374),
  ('Ex -> EA','Ex',1,'p_Ex','syntax_analyzer.py',379),
  ('Ex -> ES','Ex',1,'p_Ex','syntax_analyzer.py',380),
  ('Ex -> EL','Ex',1,'p_Ex','syntax_analyzer.py',381),
  ('Ex -> ID','Ex',1,'p_Ex','syntax_analyzer.py',382),
  ('ES -> WORDVAL','ES',1,'p_ES','syntax_analyzer.py',387),
  ('EA -> P PLUS EA','EA',3,'p_EA','syntax_analyzer.py',392),
  ('EA -> P MINUS EA','EA',3,'p_EA','syntax_analyzer.py',393),
  ('EA -> P','EA',1,'p_EA','syntax_analyzer.py',394),
  ('P -> N MULTIPLY P','P',3,'p_P','syntax_analyzer.py',399),
  ('P -> N DIVIDE P','P',3,'p_P','syntax_analyzer.py',400),
  ('P -> N','P',1,'p_P','syntax_analyzer.py',401),
  ('N -> cte','N',1,'p_N','syntax_analyzer.py',406),
  ('N -> ID','N',1,'p_N','syntax_analyzer.py',407),
  ('N -> OPENPAR EA CLOSINGPAR','N',3,'p_N','syntax_analyzer.py',408),
  ('N -> ID OPENBRACKET INTVAL CLOSINGBRACKET','N',4,'p_N','syntax_analyzer.py',409),
  ('N -> ID OPENBRACKET Idv CLOSINGBRACKET','N',4,'p_N','syntax_analyzer.py',410),
  ('cte -> INTVAL','cte',1,'p_cte','syntax_analyzer.py',415),
  ('cte -> FLOATVAL','cte',1,'p_cte','syntax_analyzer.py',416),
  ('EL -> TRUE','EL',1,'p_EL','syntax_analyzer.py',421),
  ('EL -> FALSE','EL',1,'p_EL','syntax_analyzer.py',422),
  ('EL -> OPENPAR O CLOSINGPAR Olt','EL',4,'p_EL','syntax_analyzer.py',423),
  ('Olt -> OL OPENPAR O CLOSINGPAR Olt','Olt',5,'p_Olt','syntax_analyzer.py',428),
  ('Olt -> empty','Olt',1,'p_Olt','syntax_analyzer.py',429),
  ('OL -> AND','OL',1,'p_OL','syntax_analyzer.py',434),
  ('OL -> OR','OL',1,'p_OL','syntax_analyzer.py',435),
  ('OL -> NOT','OL',1,'p_OL','syntax_analyzer.py',436),
  ('O -> WORD EQUALTO WORD','O',3,'p_O','syntax_analyzer.py',441),
  ('O -> EA OPR EA','O',3,'p_O','syntax_analyzer.py',442),
  ('O -> ID OPR ID','O',3,'p_O','syntax_analyzer.py',443),
  ('O -> ID OPR EA','O',3,'p_O','syntax_analyzer.py',444),
  ('O -> EA OPR ID','O',3,'p_O','syntax_analyzer.py',445),
  ('OPR -> EQUALTO','OPR',1,'p_OPR','syntax_analyzer.py',450),
  ('OPR -> GREATHER','OPR',1,'p_OPR','syntax_analyzer.py',451),
  ('OPR -> GREATHEREQUAL','OPR',1,'p_OPR','syntax_analyzer.py',452),
  ('OPR -> SMALLER','OPR',1,'p_OPR','syntax_analyzer.py',453),
  ('OPR -> SMALLEREQUAL','OPR',1,'p_OPR','syntax_analyzer.py',454),
  ('OPR -> NOTEQUAL','OPR',1,'p_OPR','syntax_analyzer.py',455),
  ('empty -> <empty>','empty',0,'p_empty','syntax_analyzer.py',460),
]
