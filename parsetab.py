
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'AND AS CLOSINGBRACKET CLOSINGPAR COMA COMMENT DIM DIVIDE DO EIF ELSE END EQUALS EQUALTO FALSE FLOAT FLOATVAL FOR GOSUB GREATHER GREATHEREQUAL ID IF INPUT INT INTVAL LET MINUS MULTIPLY NEXT NOT NOTEQUAL OPENBRACKET OPENPAR OR PLUS PRINT PROGRAM REPEAT RETURN SMALLER SMALLEREQUAL SUBPROCEDURE THEN TO TRUE TWOPOINTS UNTIL WEND WHILE WORD WORDVAL\n  PROGRAMA : PROGRAM V M endProgram S END\n  \n  endProgram :\n  \n    V : DIM setType Idv AS T Arr\n      | empty\n  \n  Arr : OPENBRACKET EA CLOSINGBRACKET Arr\n      | OPENBRACKET ID CLOSINGBRACKET Arr\n      | empty\n  \n  T : INT\n    | FLOAT\n    | WORD\n  \n  S : SUBPROCEDURE ID fillSub TWOPOINTS M RETURN endProcedure S\n    | empty\n  \n  fillSub :\n  \n  endProcedure :\n  \n  M : F M\n    | empty\n  \n  F : E F\n    | empty\n  \n  E : LET setType Idv EQUALS Ex\n    | DIM setType Idv AS T Arr\n    | IF EL THEN first_conditional F Esf EIF final_conditional\n    | FOR ID EQUALS EA for_assignation TO Ex for_conditional DO for_save_conditional F NEXT ID for_conditional_end\n    | WHILE while_first_conditional EL DO while_second_conditional F WEND while_final_conditional\n    | REPEAT while_first_conditional F UNTIL EL repeat_conditional\n    | GOSUB ID\n    | INPUT IDEx\n    | PRINT Ex\n  \n  for_assignation :\n  \n  for_conditional :\n  \n  for_save_conditional :\n  \n  for_conditional_end :\n  \n  repeat_conditional :\n  \n  while_first_conditional :\n  \n  while_second_conditional :\n  \n  while_final_conditional :\n  \n  first_conditional :\n  \n  second_conditional :\n  \n  final_conditional :\n  \n  Esf : ELSE second_conditional F\n      | empty\n  \n  Idv : ID COMA Idv\n      | ID\n  \n  setType :\n  \n  IDEx : ID\n       | ID OPENBRACKET setType Idv CLOSINGBRACKET\n  \n  Ex : EA\n     | EL\n     | ID\n  \n  ES : WORDVAL\n  \n  EA : EA PLUS P\n     | EA MINUS P\n     | P\n  \n  P : P MULTIPLY N\n    | P DIVIDE N\n    | N\n  \n  N : cte saveID\n    | ID saveID\n    | OPENPAR EA CLOSINGPAR\n    | ID OPENBRACKET INTVAL CLOSINGBRACKET\n    | ID OPENBRACKET setType Idv CLOSINGBRACKET\n  \n  saveID :\n  \n  cte : INTVAL\n      | FLOATVAL\n  \n  EL : TRUE\n     | FALSE \n     | OPENPAR O CLOSINGPAR\n     | OPENPAR O CLOSINGPAR OL EL\n  \n  OL : AND\n     | OR\n     | NOT\n  \n  O : Ex GREATHER Ex\n    | Ex GREATHEREQUAL Ex\n    | Ex SMALLER Ex\n    | Ex SMALLEREQUAL Ex\n    | Ex NOTEQUAL Ex\n    | Ex EQUALTO Ex\n  \n  O : Ex error Ex\n  \n  empty :\n  '
    
_lr_action_items = {'PROGRAM':([0,],[2,]),'$end':([1,70,],[0,-1,]),'DIM':([2,3,5,7,8,9,15,22,23,27,28,32,33,34,35,36,37,38,39,40,42,43,44,45,53,62,67,74,75,83,86,89,90,93,94,95,96,97,98,99,102,103,118,119,121,123,125,126,127,129,131,134,135,136,140,141,143,144,145,147,150,151,152,154,156,159,160,],[4,11,-4,11,-18,11,-33,-17,-18,-64,-65,11,-25,-26,-44,-27,-46,-47,-48,-52,-55,-61,-62,-63,-36,-57,-56,11,-66,-61,-34,-50,-51,-53,-54,-58,-78,-8,-9,-10,-19,-78,11,-32,-59,-3,-7,11,-20,-37,-67,-24,-45,-60,-38,11,-35,-78,-78,-21,-23,-5,-6,-30,11,-31,-22,]),'LET':([2,3,5,7,8,9,15,22,23,27,28,32,33,34,35,36,37,38,39,40,42,43,44,45,53,62,67,74,75,83,86,89,90,93,94,95,96,97,98,99,102,103,118,119,121,123,125,126,127,129,131,134,135,136,140,141,143,144,145,147,150,151,152,154,156,159,160,],[-78,10,-4,10,-18,10,-33,-17,-18,-64,-65,10,-25,-26,-44,-27,-46,-47,-48,-52,-55,-61,-62,-63,-36,-57,-56,10,-66,-61,-34,-50,-51,-53,-54,-58,-78,-8,-9,-10,-19,-78,10,-32,-59,-3,-7,10,-20,-37,-67,-24,-45,-60,-38,10,-35,-78,-78,-21,-23,-5,-6,-30,10,-31,-22,]),'IF':([2,3,5,7,8,9,15,22,23,27,28,32,33,34,35,36,37,38,39,40,42,43,44,45,53,62,67,74,75,83,86,89,90,93,94,95,96,97,98,99,102,103,118,119,121,123,125,126,127,129,131,134,135,136,140,141,143,144,145,147,150,151,152,154,156,159,160,],[-78,12,-4,12,-18,12,-33,-17,-18,-64,-65,12,-25,-26,-44,-27,-46,-47,-48,-52,-55,-61,-62,-63,-36,-57,-56,12,-66,-61,-34,-50,-51,-53,-54,-58,-78,-8,-9,-10,-19,-78,12,-32,-59,-3,-7,12,-20,-37,-67,-24,-45,-60,-38,12,-35,-78,-78,-21,-23,-5,-6,-30,12,-31,-22,]),'FOR':([2,3,5,7,8,9,15,22,23,27,28,32,33,34,35,36,37,38,39,40,42,43,44,45,53,62,67,74,75,83,86,89,90,93,94,95,96,97,98,99,102,103,118,119,121,123,125,126,127,129,131,134,135,136,140,141,143,144,145,147,150,151,152,154,156,159,160,],[-78,13,-4,13,-18,13,-33,-17,-18,-64,-65,13,-25,-26,-44,-27,-46,-47,-48,-52,-55,-61,-62,-63,-36,-57,-56,13,-66,-61,-34,-50,-51,-53,-54,-58,-78,-8,-9,-10,-19,-78,13,-32,-59,-3,-7,13,-20,-37,-67,-24,-45,-60,-38,13,-35,-78,-78,-21,-23,-5,-6,-30,13,-31,-22,]),'WHILE':([2,3,5,7,8,9,15,22,23,27,28,32,33,34,35,36,37,38,39,40,42,43,44,45,53,62,67,74,75,83,86,89,90,93,94,95,96,97,98,99,102,103,118,119,121,123,125,126,127,129,131,134,135,136,140,141,143,144,145,147,150,151,152,154,156,159,160,],[-78,14,-4,14,-18,14,-33,-17,-18,-64,-65,14,-25,-26,-44,-27,-46,-47,-48,-52,-55,-61,-62,-63,-36,-57,-56,14,-66,-61,-34,-50,-51,-53,-54,-58,-78,-8,-9,-10,-19,-78,14,-32,-59,-3,-7,14,-20,-37,-67,-24,-45,-60,-38,14,-35,-78,-78,-21,-23,-5,-6,-30,14,-31,-22,]),'REPEAT':([2,3,5,7,8,9,15,22,23,27,28,32,33,34,35,36,37,38,39,40,42,43,44,45,53,62,67,74,75,83,86,89,90,93,94,95,96,97,98,99,102,103,118,119,121,123,125,126,127,129,131,134,135,136,140,141,143,144,145,147,150,151,152,154,156,159,160,],[-78,15,-4,15,-18,15,-33,-17,-18,-64,-65,15,-25,-26,-44,-27,-46,-47,-48,-52,-55,-61,-62,-63,-36,-57,-56,15,-66,-61,-34,-50,-51,-53,-54,-58,-78,-8,-9,-10,-19,-78,15,-32,-59,-3,-7,15,-20,-37,-67,-24,-45,-60,-38,15,-35,-78,-78,-21,-23,-5,-6,-30,15,-31,-22,]),'GOSUB':([2,3,5,7,8,9,15,22,23,27,28,32,33,34,35,36,37,38,39,40,42,43,44,45,53,62,67,74,75,83,86,89,90,93,94,95,96,97,98,99,102,103,118,119,121,123,125,126,127,129,131,134,135,136,140,141,143,144,145,147,150,151,152,154,156,159,160,],[-78,16,-4,16,-18,16,-33,-17,-18,-64,-65,16,-25,-26,-44,-27,-46,-47,-48,-52,-55,-61,-62,-63,-36,-57,-56,16,-66,-61,-34,-50,-51,-53,-54,-58,-78,-8,-9,-10,-19,-78,16,-32,-59,-3,-7,16,-20,-37,-67,-24,-45,-60,-38,16,-35,-78,-78,-21,-23,-5,-6,-30,16,-31,-22,]),'INPUT':([2,3,5,7,8,9,15,22,23,27,28,32,33,34,35,36,37,38,39,40,42,43,44,45,53,62,67,74,75,83,86,89,90,93,94,95,96,97,98,99,102,103,118,119,121,123,125,126,127,129,131,134,135,136,140,141,143,144,145,147,150,151,152,154,156,159,160,],[-78,17,-4,17,-18,17,-33,-17,-18,-64,-65,17,-25,-26,-44,-27,-46,-47,-48,-52,-55,-61,-62,-63,-36,-57,-56,17,-66,-61,-34,-50,-51,-53,-54,-58,-78,-8,-9,-10,-19,-78,17,-32,-59,-3,-7,17,-20,-37,-67,-24,-45,-60,-38,17,-35,-78,-78,-21,-23,-5,-6,-30,17,-31,-22,]),'PRINT':([2,3,5,7,8,9,15,22,23,27,28,32,33,34,35,36,37,38,39,40,42,43,44,45,53,62,67,74,75,83,86,89,90,93,94,95,96,97,98,99,102,103,118,119,121,123,125,126,127,129,131,134,135,136,140,141,143,144,145,147,150,151,152,154,156,159,160,],[-78,18,-4,18,-18,18,-33,-17,-18,-64,-65,18,-25,-26,-44,-27,-46,-47,-48,-52,-55,-61,-62,-63,-36,-57,-56,18,-66,-61,-34,-50,-51,-53,-54,-58,-78,-8,-9,-10,-19,-78,18,-32,-59,-3,-7,18,-20,-37,-67,-24,-45,-60,-38,18,-35,-78,-78,-21,-23,-5,-6,-30,18,-31,-22,]),'SUBPROCEDURE':([2,3,5,6,7,8,9,20,21,22,23,27,28,33,34,35,36,37,38,39,40,42,43,44,45,62,67,75,83,89,90,93,94,95,96,97,98,99,102,103,119,121,123,125,127,131,134,135,136,140,143,144,145,146,147,150,151,152,153,159,160,],[-78,-78,-4,-2,-78,-16,-78,49,-15,-17,-18,-64,-65,-25,-26,-44,-27,-46,-47,-48,-52,-55,-61,-62,-63,-57,-56,-66,-61,-50,-51,-53,-54,-58,-78,-8,-9,-10,-19,-78,-32,-59,-3,-7,-20,-67,-24,-45,-60,-38,-35,-78,-78,-14,-21,-23,-5,-6,49,-31,-22,]),'END':([2,3,5,6,7,8,9,20,21,22,23,27,28,33,34,35,36,37,38,39,40,42,43,44,45,48,50,62,67,75,83,89,90,93,94,95,96,97,98,99,102,103,119,121,123,125,127,131,134,135,136,140,143,144,145,146,147,150,151,152,153,155,159,160,],[-78,-78,-4,-2,-78,-16,-78,-78,-15,-17,-18,-64,-65,-25,-26,-44,-27,-46,-47,-48,-52,-55,-61,-62,-63,70,-12,-57,-56,-66,-61,-50,-51,-53,-54,-58,-78,-8,-9,-10,-19,-78,-32,-59,-3,-7,-20,-67,-24,-45,-60,-38,-35,-78,-78,-14,-21,-23,-5,-6,-78,-11,-31,-22,]),'ID':([4,10,11,13,16,17,18,19,24,25,29,41,49,56,59,60,61,63,64,65,69,72,76,77,78,79,80,81,82,85,88,92,124,132,158,],[-43,-43,-43,30,33,35,39,47,47,47,39,39,71,83,-43,83,83,-43,83,83,47,39,39,39,39,39,39,39,39,83,47,47,138,39,159,]),'RETURN':([7,8,9,21,22,23,27,28,33,34,35,36,37,38,39,40,42,43,44,45,62,67,75,83,89,90,93,94,95,97,98,99,102,103,119,121,125,126,127,131,134,135,136,139,140,143,144,145,147,150,151,152,159,160,],[-78,-16,-78,-15,-17,-18,-64,-65,-25,-26,-44,-27,-46,-47,-48,-52,-55,-61,-62,-63,-57,-56,-66,-61,-50,-51,-53,-54,-58,-8,-9,-10,-19,-78,-32,-59,-7,-78,-20,-67,-24,-45,-60,146,-38,-35,-78,-78,-21,-23,-5,-6,-31,-22,]),'UNTIL':([9,15,22,23,27,28,32,33,34,35,36,37,38,39,40,42,43,44,45,58,62,67,75,83,89,90,93,94,95,97,98,99,102,103,119,121,125,127,131,134,135,136,140,143,144,145,147,150,151,152,159,160,],[-78,-33,-17,-18,-64,-65,-78,-25,-26,-44,-27,-46,-47,-48,-52,-55,-61,-62,-63,87,-57,-56,-66,-61,-50,-51,-53,-54,-58,-8,-9,-10,-19,-78,-32,-59,-7,-20,-67,-24,-45,-60,-38,-35,-78,-78,-21,-23,-5,-6,-31,-22,]),'ELSE':([9,22,23,27,28,33,34,35,36,37,38,39,40,42,43,44,45,53,62,67,74,75,83,89,90,93,94,95,97,98,99,102,103,104,119,121,125,127,131,134,135,136,140,143,144,145,147,150,151,152,159,160,],[-78,-17,-18,-64,-65,-25,-26,-44,-27,-46,-47,-48,-52,-55,-61,-62,-63,-36,-57,-56,-78,-66,-61,-50,-51,-53,-54,-58,-8,-9,-10,-19,-78,129,-32,-59,-7,-20,-67,-24,-45,-60,-38,-35,-78,-78,-21,-23,-5,-6,-31,-22,]),'EIF':([9,22,23,27,28,33,34,35,36,37,38,39,40,42,43,44,45,53,62,67,74,75,83,89,90,93,94,95,97,98,99,102,103,104,119,121,125,127,128,129,130,131,134,135,136,140,141,143,144,145,147,148,150,151,152,159,160,],[-78,-17,-18,-64,-65,-25,-26,-44,-27,-46,-47,-48,-52,-55,-61,-62,-63,-36,-57,-56,-78,-66,-61,-50,-51,-53,-54,-58,-8,-9,-10,-19,-78,-78,-32,-59,-7,-20,140,-37,-40,-67,-24,-45,-60,-38,-78,-35,-78,-78,-21,-39,-23,-5,-6,-31,-22,]),'WEND':([9,22,23,27,28,33,34,35,36,37,38,39,40,42,43,44,45,62,67,75,83,86,89,90,93,94,95,97,98,99,102,103,118,119,121,125,127,131,133,134,135,136,140,143,144,145,147,150,151,152,159,160,],[-78,-17,-18,-64,-65,-25,-26,-44,-27,-46,-47,-48,-52,-55,-61,-62,-63,-57,-56,-66,-61,-34,-50,-51,-53,-54,-58,-8,-9,-10,-19,-78,-78,-32,-59,-7,-20,-67,143,-24,-45,-60,-38,-35,-78,-78,-21,-23,-5,-6,-31,-22,]),'NEXT':([9,22,23,27,28,33,34,35,36,37,38,39,40,42,43,44,45,62,67,75,83,89,90,93,94,95,97,98,99,102,103,119,121,125,127,131,134,135,136,140,143,144,145,147,150,151,152,154,156,157,159,160,],[-78,-17,-18,-64,-65,-25,-26,-44,-27,-46,-47,-48,-52,-55,-61,-62,-63,-57,-56,-66,-61,-50,-51,-53,-54,-58,-8,-9,-10,-19,-78,-32,-59,-7,-20,-67,-24,-45,-60,-38,-35,-78,-78,-21,-23,-5,-6,-30,-78,158,-31,-22,]),'TRUE':([12,14,18,29,31,41,72,76,77,78,79,80,81,82,87,105,106,107,108,132,],[27,-33,27,27,27,27,27,27,27,27,27,27,27,27,27,27,-68,-69,-70,27,]),'FALSE':([12,14,18,29,31,41,72,76,77,78,79,80,81,82,87,105,106,107,108,132,],[28,-33,28,28,28,28,28,28,28,28,28,28,28,28,28,28,-68,-69,-70,28,]),'OPENPAR':([12,14,18,29,31,41,56,60,61,64,65,72,76,77,78,79,80,81,82,85,87,105,106,107,108,124,132,],[29,-33,41,41,29,41,85,85,85,85,85,41,41,41,41,41,41,41,41,85,29,29,-68,-69,-70,85,41,]),'INTVAL':([18,29,41,56,60,61,63,64,65,72,76,77,78,79,80,81,82,85,124,132,],[44,44,44,44,44,44,91,44,44,44,44,44,44,44,44,44,44,44,44,44,]),'FLOATVAL':([18,29,41,56,60,61,64,65,72,76,77,78,79,80,81,82,85,124,132,],[45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,]),'THEN':([26,27,28,75,131,],[53,-64,-65,-66,-67,]),'GREATHER':([27,28,37,38,39,40,42,43,44,45,55,62,66,67,75,83,89,90,93,94,95,121,131,136,],[-64,-65,-46,-47,-48,-52,-55,-61,-62,-63,76,-57,-46,-56,-66,-61,-50,-51,-53,-54,-58,-59,-67,-60,]),'GREATHEREQUAL':([27,28,37,38,39,40,42,43,44,45,55,62,66,67,75,83,89,90,93,94,95,121,131,136,],[-64,-65,-46,-47,-48,-52,-55,-61,-62,-63,77,-57,-46,-56,-66,-61,-50,-51,-53,-54,-58,-59,-67,-60,]),'SMALLER':([27,28,37,38,39,40,42,43,44,45,55,62,66,67,75,83,89,90,93,94,95,121,131,136,],[-64,-65,-46,-47,-48,-52,-55,-61,-62,-63,78,-57,-46,-56,-66,-61,-50,-51,-53,-54,-58,-59,-67,-60,]),'SMALLEREQUAL':([27,28,37,38,39,40,42,43,44,45,55,62,66,67,75,83,89,90,93,94,95,121,131,136,],[-64,-65,-46,-47,-48,-52,-55,-61,-62,-63,79,-57,-46,-56,-66,-61,-50,-51,-53,-54,-58,-59,-67,-60,]),'NOTEQUAL':([27,28,37,38,39,40,42,43,44,45,55,62,66,67,75,83,89,90,93,94,95,121,131,136,],[-64,-65,-46,-47,-48,-52,-55,-61,-62,-63,80,-57,-46,-56,-66,-61,-50,-51,-53,-54,-58,-59,-67,-60,]),'EQUALTO':([27,28,37,38,39,40,42,43,44,45,55,62,66,67,75,83,89,90,93,94,95,121,131,136,],[-64,-65,-46,-47,-48,-52,-55,-61,-62,-63,81,-57,-46,-56,-66,-61,-50,-51,-53,-54,-58,-59,-67,-60,]),'error':([27,28,37,38,39,40,42,43,44,45,55,62,66,67,75,83,89,90,93,94,95,121,131,136,],[-64,-65,-46,-47,-48,-52,-55,-61,-62,-63,82,-57,-46,-56,-66,-61,-50,-51,-53,-54,-58,-59,-67,-60,]),'DO':([27,28,37,38,39,40,42,43,44,45,57,62,67,75,83,89,90,93,94,95,121,131,136,142,149,],[-64,-65,-46,-47,-48,-52,-55,-61,-62,-63,86,-57,-56,-66,-61,-50,-51,-53,-54,-58,-59,-67,-60,-29,154,]),'CLOSINGPAR':([27,28,37,38,39,40,42,43,44,45,54,62,66,67,75,83,89,90,93,94,95,109,110,111,112,113,114,115,117,121,131,136,],[-64,-65,-46,-47,-48,-52,-55,-61,-62,-63,75,-57,95,-56,-66,-61,-50,-51,-53,-54,-58,-71,-72,-73,-74,-75,-76,-77,95,-59,-67,-60,]),'EQUALS':([30,47,51,100,],[56,-42,72,-41,]),'OPENBRACKET':([35,39,83,96,97,98,99,103,138,144,145,],[59,63,63,124,-8,-9,-10,124,63,124,124,]),'PLUS':([37,39,40,42,43,44,45,62,66,67,83,84,89,90,93,94,95,117,121,136,137,138,],[60,-61,-52,-55,-61,-62,-63,-57,60,-56,-61,60,-50,-51,-53,-54,-58,60,-59,-60,60,-61,]),'MINUS':([37,39,40,42,43,44,45,62,66,67,83,84,89,90,93,94,95,117,121,136,137,138,],[61,-61,-52,-55,-61,-62,-63,-57,61,-56,-61,61,-50,-51,-53,-54,-58,61,-59,-60,61,-61,]),'MULTIPLY':([39,40,42,43,44,45,62,67,83,89,90,93,94,95,121,136,138,],[-61,64,-55,-61,-62,-63,-57,-56,-61,64,64,-53,-54,-58,-59,-60,-61,]),'DIVIDE':([39,40,42,43,44,45,62,67,83,89,90,93,94,95,121,136,138,],[-61,65,-55,-61,-62,-63,-57,-56,-61,65,65,-53,-54,-58,-59,-60,-61,]),'TO':([40,42,43,44,45,62,67,83,84,89,90,93,94,95,116,121,136,],[-52,-55,-61,-62,-63,-57,-56,-61,-28,-50,-51,-53,-54,-58,132,-59,-60,]),'CLOSINGBRACKET':([40,42,43,44,45,47,62,67,83,89,90,91,93,94,95,100,120,121,122,136,137,138,],[-52,-55,-61,-62,-63,-42,-57,-56,-61,-50,-51,121,-53,-54,-58,-41,135,-59,136,-60,144,145,]),'AS':([46,47,52,100,],[68,-42,73,-41,]),'COMA':([47,],[69,]),'INT':([68,73,],[97,97,]),'FLOAT':([68,73,],[98,98,]),'WORD':([68,73,],[99,99,]),'TWOPOINTS':([71,101,],[-13,126,]),'AND':([75,],[106,]),'OR':([75,],[107,]),'NOT':([75,],[108,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'PROGRAMA':([0,],[1,]),'V':([2,],[3,]),'empty':([2,3,7,9,20,32,74,96,103,104,118,126,141,144,145,153,156,],[5,8,8,23,50,23,23,125,125,130,23,8,23,125,125,50,23,]),'M':([3,7,126,],[6,21,139,]),'F':([3,7,9,32,74,118,126,141,156,],[7,7,22,58,104,133,7,148,157,]),'E':([3,7,9,32,74,118,126,141,156,],[9,9,9,9,9,9,9,9,9,]),'setType':([4,10,11,59,63,],[19,24,25,88,92,]),'endProgram':([6,],[20,]),'EL':([12,18,29,31,41,72,76,77,78,79,80,81,82,87,105,132,],[26,38,38,57,38,38,38,38,38,38,38,38,38,119,131,38,]),'while_first_conditional':([14,15,],[31,32,]),'IDEx':([17,],[34,]),'Ex':([18,29,41,72,76,77,78,79,80,81,82,132,],[36,55,55,102,109,110,111,112,113,114,115,142,]),'EA':([18,29,41,56,72,76,77,78,79,80,81,82,85,124,132,],[37,37,66,84,37,37,37,37,37,37,37,37,117,137,37,]),'P':([18,29,41,56,60,61,72,76,77,78,79,80,81,82,85,124,132,],[40,40,40,40,89,90,40,40,40,40,40,40,40,40,40,40,40,]),'N':([18,29,41,56,60,61,64,65,72,76,77,78,79,80,81,82,85,124,132,],[42,42,42,42,42,42,93,94,42,42,42,42,42,42,42,42,42,42,42,]),'cte':([18,29,41,56,60,61,64,65,72,76,77,78,79,80,81,82,85,124,132,],[43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,]),'Idv':([19,24,25,69,88,92,],[46,51,52,100,120,122,]),'S':([20,153,],[48,155,]),'O':([29,41,],[54,54,]),'saveID':([39,43,83,138,],[62,67,62,62,]),'first_conditional':([53,],[74,]),'T':([68,73,],[96,103,]),'fillSub':([71,],[101,]),'OL':([75,],[105,]),'for_assignation':([84,],[116,]),'while_second_conditional':([86,],[118,]),'Arr':([96,103,144,145,],[123,127,151,152,]),'Esf':([104,],[128,]),'repeat_conditional':([119,],[134,]),'second_conditional':([129,],[141,]),'final_conditional':([140,],[147,]),'for_conditional':([142,],[149,]),'while_final_conditional':([143,],[150,]),'endProcedure':([146,],[153,]),'for_save_conditional':([154,],[156,]),'for_conditional_end':([159,],[160,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> PROGRAMA","S'",1,None,None,None),
  ('PROGRAMA -> PROGRAM V M endProgram S END','PROGRAMA',6,'p_PROGRAMA','syntax_analyzer.py',363),
  ('endProgram -> <empty>','endProgram',0,'p_endProgram','syntax_analyzer.py',374),
  ('V -> DIM setType Idv AS T Arr','V',6,'p_V','syntax_analyzer.py',381),
  ('V -> empty','V',1,'p_V','syntax_analyzer.py',382),
  ('Arr -> OPENBRACKET EA CLOSINGBRACKET Arr','Arr',4,'p_Arr','syntax_analyzer.py',391),
  ('Arr -> OPENBRACKET ID CLOSINGBRACKET Arr','Arr',4,'p_Arr','syntax_analyzer.py',392),
  ('Arr -> empty','Arr',1,'p_Arr','syntax_analyzer.py',393),
  ('T -> INT','T',1,'p_T','syntax_analyzer.py',398),
  ('T -> FLOAT','T',1,'p_T','syntax_analyzer.py',399),
  ('T -> WORD','T',1,'p_T','syntax_analyzer.py',400),
  ('S -> SUBPROCEDURE ID fillSub TWOPOINTS M RETURN endProcedure S','S',8,'p_S','syntax_analyzer.py',406),
  ('S -> empty','S',1,'p_S','syntax_analyzer.py',407),
  ('fillSub -> <empty>','fillSub',0,'p_fillSub','syntax_analyzer.py',417),
  ('endProcedure -> <empty>','endProcedure',0,'p_endProcedure','syntax_analyzer.py',424),
  ('M -> F M','M',2,'p_M','syntax_analyzer.py',433),
  ('M -> empty','M',1,'p_M','syntax_analyzer.py',434),
  ('F -> E F','F',2,'p_F','syntax_analyzer.py',439),
  ('F -> empty','F',1,'p_F','syntax_analyzer.py',440),
  ('E -> LET setType Idv EQUALS Ex','E',5,'p_E','syntax_analyzer.py',445),
  ('E -> DIM setType Idv AS T Arr','E',6,'p_E','syntax_analyzer.py',446),
  ('E -> IF EL THEN first_conditional F Esf EIF final_conditional','E',8,'p_E','syntax_analyzer.py',447),
  ('E -> FOR ID EQUALS EA for_assignation TO Ex for_conditional DO for_save_conditional F NEXT ID for_conditional_end','E',14,'p_E','syntax_analyzer.py',448),
  ('E -> WHILE while_first_conditional EL DO while_second_conditional F WEND while_final_conditional','E',8,'p_E','syntax_analyzer.py',449),
  ('E -> REPEAT while_first_conditional F UNTIL EL repeat_conditional','E',6,'p_E','syntax_analyzer.py',450),
  ('E -> GOSUB ID','E',2,'p_E','syntax_analyzer.py',451),
  ('E -> INPUT IDEx','E',2,'p_E','syntax_analyzer.py',452),
  ('E -> PRINT Ex','E',2,'p_E','syntax_analyzer.py',453),
  ('for_assignation -> <empty>','for_assignation',0,'p_for_assignation','syntax_analyzer.py',477),
  ('for_conditional -> <empty>','for_conditional',0,'p_for_conditional','syntax_analyzer.py',486),
  ('for_save_conditional -> <empty>','for_save_conditional',0,'p_for_save_conditional','syntax_analyzer.py',497),
  ('for_conditional_end -> <empty>','for_conditional_end',0,'p_for_conditional_end','syntax_analyzer.py',509),
  ('repeat_conditional -> <empty>','repeat_conditional',0,'p_repeat_conditional','syntax_analyzer.py',524),
  ('while_first_conditional -> <empty>','while_first_conditional',0,'p_while_first_conditional','syntax_analyzer.py',534),
  ('while_second_conditional -> <empty>','while_second_conditional',0,'p_while_second_conditional','syntax_analyzer.py',543),
  ('while_final_conditional -> <empty>','while_final_conditional',0,'p_while_final_conditional','syntax_analyzer.py',555),
  ('first_conditional -> <empty>','first_conditional',0,'p_first_conditional','syntax_analyzer.py',568),
  ('second_conditional -> <empty>','second_conditional',0,'p_second_conditional','syntax_analyzer.py',579),
  ('final_conditional -> <empty>','final_conditional',0,'p_final_conditional','syntax_analyzer.py',591),
  ('Esf -> ELSE second_conditional F','Esf',3,'p_Esf','syntax_analyzer.py',599),
  ('Esf -> empty','Esf',1,'p_Esf','syntax_analyzer.py',600),
  ('Idv -> ID COMA Idv','Idv',3,'p_Idv','syntax_analyzer.py',605),
  ('Idv -> ID','Idv',1,'p_Idv','syntax_analyzer.py',606),
  ('setType -> <empty>','setType',0,'p_setType','syntax_analyzer.py',618),
  ('IDEx -> ID','IDEx',1,'p_IDEx','syntax_analyzer.py',625),
  ('IDEx -> ID OPENBRACKET setType Idv CLOSINGBRACKET','IDEx',5,'p_IDEx','syntax_analyzer.py',626),
  ('Ex -> EA','Ex',1,'p_Ex','syntax_analyzer.py',632),
  ('Ex -> EL','Ex',1,'p_Ex','syntax_analyzer.py',633),
  ('Ex -> ID','Ex',1,'p_Ex','syntax_analyzer.py',634),
  ('ES -> WORDVAL','ES',1,'p_ES','syntax_analyzer.py',640),
  ('EA -> EA PLUS P','EA',3,'p_EA','syntax_analyzer.py',645),
  ('EA -> EA MINUS P','EA',3,'p_EA','syntax_analyzer.py',646),
  ('EA -> P','EA',1,'p_EA','syntax_analyzer.py',647),
  ('P -> P MULTIPLY N','P',3,'p_P','syntax_analyzer.py',670),
  ('P -> P DIVIDE N','P',3,'p_P','syntax_analyzer.py',671),
  ('P -> N','P',1,'p_P','syntax_analyzer.py',672),
  ('N -> cte saveID','N',2,'p_N','syntax_analyzer.py',696),
  ('N -> ID saveID','N',2,'p_N','syntax_analyzer.py',697),
  ('N -> OPENPAR EA CLOSINGPAR','N',3,'p_N','syntax_analyzer.py',698),
  ('N -> ID OPENBRACKET INTVAL CLOSINGBRACKET','N',4,'p_N','syntax_analyzer.py',699),
  ('N -> ID OPENBRACKET setType Idv CLOSINGBRACKET','N',5,'p_N','syntax_analyzer.py',700),
  ('saveID -> <empty>','saveID',0,'p_saveID','syntax_analyzer.py',709),
  ('cte -> INTVAL','cte',1,'p_cte','syntax_analyzer.py',729),
  ('cte -> FLOATVAL','cte',1,'p_cte','syntax_analyzer.py',730),
  ('EL -> TRUE','EL',1,'p_EL','syntax_analyzer.py',736),
  ('EL -> FALSE','EL',1,'p_EL','syntax_analyzer.py',737),
  ('EL -> OPENPAR O CLOSINGPAR','EL',3,'p_EL','syntax_analyzer.py',738),
  ('EL -> OPENPAR O CLOSINGPAR OL EL','EL',5,'p_EL','syntax_analyzer.py',739),
  ('OL -> AND','OL',1,'p_OL','syntax_analyzer.py',757),
  ('OL -> OR','OL',1,'p_OL','syntax_analyzer.py',758),
  ('OL -> NOT','OL',1,'p_OL','syntax_analyzer.py',759),
  ('O -> Ex GREATHER Ex','O',3,'p_O','syntax_analyzer.py',767),
  ('O -> Ex GREATHEREQUAL Ex','O',3,'p_O','syntax_analyzer.py',768),
  ('O -> Ex SMALLER Ex','O',3,'p_O','syntax_analyzer.py',769),
  ('O -> Ex SMALLEREQUAL Ex','O',3,'p_O','syntax_analyzer.py',770),
  ('O -> Ex NOTEQUAL Ex','O',3,'p_O','syntax_analyzer.py',771),
  ('O -> Ex EQUALTO Ex','O',3,'p_O','syntax_analyzer.py',772),
  ('O -> Ex error Ex','O',3,'p_O_error','syntax_analyzer.py',799),
  ('empty -> <empty>','empty',0,'p_empty','syntax_analyzer.py',805),
]
