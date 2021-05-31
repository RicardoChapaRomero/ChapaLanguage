
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'AND AS CLOSINGBRACKET CLOSINGPAR COMA COMMENT DIM DIVIDE DO EIF ELSE END EQUALS EQUALTO FALSE FLOAT FLOATVAL FOR GOSUB GREATHER GREATHEREQUAL ID IF INPUT INT INTVAL LET MINUS MULTIPLY NEXT NOT NOTEQUAL OPENBRACKET OPENPAR OR PLUS PRINT PROGRAM REPEAT RETURN SMALLER SMALLEREQUAL SUBPROCEDURE THEN TO TRUE TWOPOINTS UNTIL WEND WHILE WORD WORDVAL\n  PROGRAMA : PROGRAM V M endProgram S END\n  \n  endProgram :\n  \n    V : DIM setType Idv AS T Arr\n      | empty\n  \n  Arr : OPENBRACKET arr_space CLOSINGBRACKET\n      | empty\n  \n  arr_space : EA COMA arr_space\n            | ID COMA arr_space\n            | EA\n            | ID\n  \n  T : INT\n    | FLOAT\n    | WORD\n  \n  S : SUBPROCEDURE ID fillSub TWOPOINTS M RETURN endProcedure S\n    | empty\n  \n  fillSub :\n  \n  endProcedure :\n  \n  M : F M\n    | empty\n  \n  F : E F\n    | empty\n  \n  E : LET setType Idv EQUALS Ex\n    | DIM setType Idv AS T Arr\n    | IF EL THEN first_conditional F Esf EIF final_conditional\n    | FOR ID EQUALS EA for_assignation TO Ex for_conditional DO for_save_conditional F NEXT ID for_conditional_end\n    | WHILE while_first_conditional EL DO while_second_conditional F WEND while_final_conditional\n    | REPEAT while_first_conditional F UNTIL EL repeat_conditional\n    | GOSUB ID\n    | INPUT IDEx\n    | PRINT Ex\n  \n  for_assignation :\n  \n  for_conditional :\n  \n  for_save_conditional :\n  \n  for_conditional_end :\n  \n  repeat_conditional :\n  \n  while_first_conditional :\n  \n  while_second_conditional :\n  \n  while_final_conditional :\n  \n  first_conditional :\n  \n  second_conditional :\n  \n  final_conditional :\n  \n  Esf : ELSE second_conditional F\n      | empty\n  \n  Idv : ID COMA Idv\n      | ID\n      | ID Arr\n  \n  setType :\n  \n  IDEx : ID\n       | ID OPENBRACKET setType Idv CLOSINGBRACKET\n  \n  Ex : EA\n     | EL\n     | ID\n  \n  ES : WORDVAL\n  \n  EA : EA PLUS P\n     | EA MINUS P\n     | P\n  \n  P : P MULTIPLY N\n    | P DIVIDE N\n    | N\n  \n  N : cte saveID\n    | ID saveID\n    | OPENPAR EA CLOSINGPAR\n    | ID Arr\n  \n  saveID :\n  \n  cte : INTVAL\n      | FLOATVAL\n  \n  EL : TRUE\n     | FALSE \n     | OPENPAR O CLOSINGPAR\n     | OPENPAR O CLOSINGPAR OL EL\n  \n  OL : AND\n     | OR\n     | NOT\n  \n  O : Ex GREATHER Ex\n    | Ex GREATHEREQUAL Ex\n    | Ex SMALLER Ex\n    | Ex SMALLEREQUAL Ex\n    | Ex NOTEQUAL Ex\n    | Ex EQUALTO Ex\n  \n  O : Ex error Ex\n  \n  empty :\n  '
    
_lr_action_items = {'PROGRAM':([0,],[2,]),'$end':([1,73,],[0,-1,]),'DIM':([2,3,5,7,8,9,15,22,23,27,28,32,33,34,35,36,37,38,39,40,42,43,44,45,53,62,63,65,69,77,78,86,89,92,93,97,98,99,100,101,102,103,106,107,122,123,125,128,129,130,132,134,137,138,142,143,145,147,150,152,154,157,158,],[4,11,-4,11,-21,11,-36,-20,-21,-67,-68,11,-28,-29,-48,-30,-50,-51,-52,-56,-59,-64,-65,-66,-39,-61,-63,-6,-60,11,-69,-64,-37,-54,-55,-57,-58,-62,-81,-11,-12,-13,-22,-81,11,-35,-5,-3,11,-23,-40,-70,-27,-49,-41,11,-38,-24,-26,-33,11,-34,-25,]),'LET':([2,3,5,7,8,9,15,22,23,27,28,32,33,34,35,36,37,38,39,40,42,43,44,45,53,62,63,65,69,77,78,86,89,92,93,97,98,99,100,101,102,103,106,107,122,123,125,128,129,130,132,134,137,138,142,143,145,147,150,152,154,157,158,],[-81,10,-4,10,-21,10,-36,-20,-21,-67,-68,10,-28,-29,-48,-30,-50,-51,-52,-56,-59,-64,-65,-66,-39,-61,-63,-6,-60,10,-69,-64,-37,-54,-55,-57,-58,-62,-81,-11,-12,-13,-22,-81,10,-35,-5,-3,10,-23,-40,-70,-27,-49,-41,10,-38,-24,-26,-33,10,-34,-25,]),'IF':([2,3,5,7,8,9,15,22,23,27,28,32,33,34,35,36,37,38,39,40,42,43,44,45,53,62,63,65,69,77,78,86,89,92,93,97,98,99,100,101,102,103,106,107,122,123,125,128,129,130,132,134,137,138,142,143,145,147,150,152,154,157,158,],[-81,12,-4,12,-21,12,-36,-20,-21,-67,-68,12,-28,-29,-48,-30,-50,-51,-52,-56,-59,-64,-65,-66,-39,-61,-63,-6,-60,12,-69,-64,-37,-54,-55,-57,-58,-62,-81,-11,-12,-13,-22,-81,12,-35,-5,-3,12,-23,-40,-70,-27,-49,-41,12,-38,-24,-26,-33,12,-34,-25,]),'FOR':([2,3,5,7,8,9,15,22,23,27,28,32,33,34,35,36,37,38,39,40,42,43,44,45,53,62,63,65,69,77,78,86,89,92,93,97,98,99,100,101,102,103,106,107,122,123,125,128,129,130,132,134,137,138,142,143,145,147,150,152,154,157,158,],[-81,13,-4,13,-21,13,-36,-20,-21,-67,-68,13,-28,-29,-48,-30,-50,-51,-52,-56,-59,-64,-65,-66,-39,-61,-63,-6,-60,13,-69,-64,-37,-54,-55,-57,-58,-62,-81,-11,-12,-13,-22,-81,13,-35,-5,-3,13,-23,-40,-70,-27,-49,-41,13,-38,-24,-26,-33,13,-34,-25,]),'WHILE':([2,3,5,7,8,9,15,22,23,27,28,32,33,34,35,36,37,38,39,40,42,43,44,45,53,62,63,65,69,77,78,86,89,92,93,97,98,99,100,101,102,103,106,107,122,123,125,128,129,130,132,134,137,138,142,143,145,147,150,152,154,157,158,],[-81,14,-4,14,-21,14,-36,-20,-21,-67,-68,14,-28,-29,-48,-30,-50,-51,-52,-56,-59,-64,-65,-66,-39,-61,-63,-6,-60,14,-69,-64,-37,-54,-55,-57,-58,-62,-81,-11,-12,-13,-22,-81,14,-35,-5,-3,14,-23,-40,-70,-27,-49,-41,14,-38,-24,-26,-33,14,-34,-25,]),'REPEAT':([2,3,5,7,8,9,15,22,23,27,28,32,33,34,35,36,37,38,39,40,42,43,44,45,53,62,63,65,69,77,78,86,89,92,93,97,98,99,100,101,102,103,106,107,122,123,125,128,129,130,132,134,137,138,142,143,145,147,150,152,154,157,158,],[-81,15,-4,15,-21,15,-36,-20,-21,-67,-68,15,-28,-29,-48,-30,-50,-51,-52,-56,-59,-64,-65,-66,-39,-61,-63,-6,-60,15,-69,-64,-37,-54,-55,-57,-58,-62,-81,-11,-12,-13,-22,-81,15,-35,-5,-3,15,-23,-40,-70,-27,-49,-41,15,-38,-24,-26,-33,15,-34,-25,]),'GOSUB':([2,3,5,7,8,9,15,22,23,27,28,32,33,34,35,36,37,38,39,40,42,43,44,45,53,62,63,65,69,77,78,86,89,92,93,97,98,99,100,101,102,103,106,107,122,123,125,128,129,130,132,134,137,138,142,143,145,147,150,152,154,157,158,],[-81,16,-4,16,-21,16,-36,-20,-21,-67,-68,16,-28,-29,-48,-30,-50,-51,-52,-56,-59,-64,-65,-66,-39,-61,-63,-6,-60,16,-69,-64,-37,-54,-55,-57,-58,-62,-81,-11,-12,-13,-22,-81,16,-35,-5,-3,16,-23,-40,-70,-27,-49,-41,16,-38,-24,-26,-33,16,-34,-25,]),'INPUT':([2,3,5,7,8,9,15,22,23,27,28,32,33,34,35,36,37,38,39,40,42,43,44,45,53,62,63,65,69,77,78,86,89,92,93,97,98,99,100,101,102,103,106,107,122,123,125,128,129,130,132,134,137,138,142,143,145,147,150,152,154,157,158,],[-81,17,-4,17,-21,17,-36,-20,-21,-67,-68,17,-28,-29,-48,-30,-50,-51,-52,-56,-59,-64,-65,-66,-39,-61,-63,-6,-60,17,-69,-64,-37,-54,-55,-57,-58,-62,-81,-11,-12,-13,-22,-81,17,-35,-5,-3,17,-23,-40,-70,-27,-49,-41,17,-38,-24,-26,-33,17,-34,-25,]),'PRINT':([2,3,5,7,8,9,15,22,23,27,28,32,33,34,35,36,37,38,39,40,42,43,44,45,53,62,63,65,69,77,78,86,89,92,93,97,98,99,100,101,102,103,106,107,122,123,125,128,129,130,132,134,137,138,142,143,145,147,150,152,154,157,158,],[-81,18,-4,18,-21,18,-36,-20,-21,-67,-68,18,-28,-29,-48,-30,-50,-51,-52,-56,-59,-64,-65,-66,-39,-61,-63,-6,-60,18,-69,-64,-37,-54,-55,-57,-58,-62,-81,-11,-12,-13,-22,-81,18,-35,-5,-3,18,-23,-40,-70,-27,-49,-41,18,-38,-24,-26,-33,18,-34,-25,]),'SUBPROCEDURE':([2,3,5,6,7,8,9,20,21,22,23,27,28,33,34,35,36,37,38,39,40,42,43,44,45,62,63,65,69,78,86,92,93,97,98,99,100,101,102,103,106,107,123,125,128,130,134,137,138,142,145,146,147,150,151,157,158,],[-81,-81,-4,-2,-81,-19,-81,49,-18,-20,-21,-67,-68,-28,-29,-48,-30,-50,-51,-52,-56,-59,-64,-65,-66,-61,-63,-6,-60,-69,-64,-54,-55,-57,-58,-62,-81,-11,-12,-13,-22,-81,-35,-5,-3,-23,-70,-27,-49,-41,-38,-17,-24,-26,49,-34,-25,]),'END':([2,3,5,6,7,8,9,20,21,22,23,27,28,33,34,35,36,37,38,39,40,42,43,44,45,48,50,62,63,65,69,78,86,92,93,97,98,99,100,101,102,103,106,107,123,125,128,130,134,137,138,142,145,146,147,150,151,153,157,158,],[-81,-81,-4,-2,-81,-19,-81,-81,-18,-20,-21,-67,-68,-28,-29,-48,-30,-50,-51,-52,-56,-59,-64,-65,-66,73,-15,-61,-63,-6,-60,-69,-64,-54,-55,-57,-58,-62,-81,-11,-12,-13,-22,-81,-35,-5,-3,-23,-70,-27,-49,-41,-38,-17,-24,-26,-81,-14,-34,-25,]),'ID':([4,10,11,13,16,17,18,19,24,25,29,41,49,56,59,60,61,64,66,67,71,75,79,80,81,82,83,84,85,88,91,126,127,135,156,],[-47,-47,-47,30,33,35,39,47,47,47,39,39,74,86,-47,86,86,96,86,86,47,39,39,39,39,39,39,39,39,86,47,96,96,39,157,]),'RETURN':([7,8,9,21,22,23,27,28,33,34,35,36,37,38,39,40,42,43,44,45,62,63,65,69,78,86,92,93,97,98,99,101,102,103,106,107,123,125,129,130,134,137,138,141,142,145,147,150,157,158,],[-81,-19,-81,-18,-20,-21,-67,-68,-28,-29,-48,-30,-50,-51,-52,-56,-59,-64,-65,-66,-61,-63,-6,-60,-69,-64,-54,-55,-57,-58,-62,-11,-12,-13,-22,-81,-35,-5,-81,-23,-70,-27,-49,146,-41,-38,-24,-26,-34,-25,]),'UNTIL':([9,15,22,23,27,28,32,33,34,35,36,37,38,39,40,42,43,44,45,58,62,63,65,69,78,86,92,93,97,98,99,101,102,103,106,107,123,125,130,134,137,138,142,145,147,150,157,158,],[-81,-36,-20,-21,-67,-68,-81,-28,-29,-48,-30,-50,-51,-52,-56,-59,-64,-65,-66,90,-61,-63,-6,-60,-69,-64,-54,-55,-57,-58,-62,-11,-12,-13,-22,-81,-35,-5,-23,-70,-27,-49,-41,-38,-24,-26,-34,-25,]),'ELSE':([9,22,23,27,28,33,34,35,36,37,38,39,40,42,43,44,45,53,62,63,65,69,77,78,86,92,93,97,98,99,101,102,103,106,107,108,123,125,130,134,137,138,142,145,147,150,157,158,],[-81,-20,-21,-67,-68,-28,-29,-48,-30,-50,-51,-52,-56,-59,-64,-65,-66,-39,-61,-63,-6,-60,-81,-69,-64,-54,-55,-57,-58,-62,-11,-12,-13,-22,-81,132,-35,-5,-23,-70,-27,-49,-41,-38,-24,-26,-34,-25,]),'EIF':([9,22,23,27,28,33,34,35,36,37,38,39,40,42,43,44,45,53,62,63,65,69,77,78,86,92,93,97,98,99,101,102,103,106,107,108,123,125,130,131,132,133,134,137,138,142,143,145,147,148,150,157,158,],[-81,-20,-21,-67,-68,-28,-29,-48,-30,-50,-51,-52,-56,-59,-64,-65,-66,-39,-61,-63,-6,-60,-81,-69,-64,-54,-55,-57,-58,-62,-11,-12,-13,-22,-81,-81,-35,-5,-23,142,-40,-43,-70,-27,-49,-41,-81,-38,-24,-42,-26,-34,-25,]),'WEND':([9,22,23,27,28,33,34,35,36,37,38,39,40,42,43,44,45,62,63,65,69,78,86,89,92,93,97,98,99,101,102,103,106,107,122,123,125,130,134,136,137,138,142,145,147,150,157,158,],[-81,-20,-21,-67,-68,-28,-29,-48,-30,-50,-51,-52,-56,-59,-64,-65,-66,-61,-63,-6,-60,-69,-64,-37,-54,-55,-57,-58,-62,-11,-12,-13,-22,-81,-81,-35,-5,-23,-70,145,-27,-49,-41,-38,-24,-26,-34,-25,]),'NEXT':([9,22,23,27,28,33,34,35,36,37,38,39,40,42,43,44,45,62,63,65,69,78,86,92,93,97,98,99,101,102,103,106,107,123,125,130,134,137,138,142,145,147,150,152,154,155,157,158,],[-81,-20,-21,-67,-68,-28,-29,-48,-30,-50,-51,-52,-56,-59,-64,-65,-66,-61,-63,-6,-60,-69,-64,-54,-55,-57,-58,-62,-11,-12,-13,-22,-81,-35,-5,-23,-70,-27,-49,-41,-38,-24,-26,-33,-81,156,-34,-25,]),'TRUE':([12,14,18,29,31,41,75,79,80,81,82,83,84,85,90,109,110,111,112,135,],[27,-36,27,27,27,27,27,27,27,27,27,27,27,27,27,27,-71,-72,-73,27,]),'FALSE':([12,14,18,29,31,41,75,79,80,81,82,83,84,85,90,109,110,111,112,135,],[28,-36,28,28,28,28,28,28,28,28,28,28,28,28,28,28,-71,-72,-73,28,]),'OPENPAR':([12,14,18,29,31,41,56,60,61,64,66,67,75,79,80,81,82,83,84,85,88,90,109,110,111,112,126,127,135,],[29,-36,41,41,29,41,88,88,88,88,88,88,41,41,41,41,41,41,41,41,88,29,29,-71,-72,-73,88,88,41,]),'INTVAL':([18,29,41,56,60,61,64,66,67,75,79,80,81,82,83,84,85,88,126,127,135,],[44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,]),'FLOATVAL':([18,29,41,56,60,61,64,66,67,75,79,80,81,82,83,84,85,88,126,127,135,],[45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,]),'THEN':([26,27,28,78,134,],[53,-67,-68,-69,-70,]),'GREATHER':([27,28,37,38,39,40,42,43,44,45,55,62,63,65,68,69,78,86,92,93,97,98,99,125,134,],[-67,-68,-50,-51,-52,-56,-59,-64,-65,-66,79,-61,-63,-6,-50,-60,-69,-64,-54,-55,-57,-58,-62,-5,-70,]),'GREATHEREQUAL':([27,28,37,38,39,40,42,43,44,45,55,62,63,65,68,69,78,86,92,93,97,98,99,125,134,],[-67,-68,-50,-51,-52,-56,-59,-64,-65,-66,80,-61,-63,-6,-50,-60,-69,-64,-54,-55,-57,-58,-62,-5,-70,]),'SMALLER':([27,28,37,38,39,40,42,43,44,45,55,62,63,65,68,69,78,86,92,93,97,98,99,125,134,],[-67,-68,-50,-51,-52,-56,-59,-64,-65,-66,81,-61,-63,-6,-50,-60,-69,-64,-54,-55,-57,-58,-62,-5,-70,]),'SMALLEREQUAL':([27,28,37,38,39,40,42,43,44,45,55,62,63,65,68,69,78,86,92,93,97,98,99,125,134,],[-67,-68,-50,-51,-52,-56,-59,-64,-65,-66,82,-61,-63,-6,-50,-60,-69,-64,-54,-55,-57,-58,-62,-5,-70,]),'NOTEQUAL':([27,28,37,38,39,40,42,43,44,45,55,62,63,65,68,69,78,86,92,93,97,98,99,125,134,],[-67,-68,-50,-51,-52,-56,-59,-64,-65,-66,83,-61,-63,-6,-50,-60,-69,-64,-54,-55,-57,-58,-62,-5,-70,]),'EQUALTO':([27,28,37,38,39,40,42,43,44,45,55,62,63,65,68,69,78,86,92,93,97,98,99,125,134,],[-67,-68,-50,-51,-52,-56,-59,-64,-65,-66,84,-61,-63,-6,-50,-60,-69,-64,-54,-55,-57,-58,-62,-5,-70,]),'error':([27,28,37,38,39,40,42,43,44,45,55,62,63,65,68,69,78,86,92,93,97,98,99,125,134,],[-67,-68,-50,-51,-52,-56,-59,-64,-65,-66,85,-61,-63,-6,-50,-60,-69,-64,-54,-55,-57,-58,-62,-5,-70,]),'DO':([27,28,37,38,39,40,42,43,44,45,57,62,63,65,69,78,86,92,93,97,98,99,125,134,144,149,],[-67,-68,-50,-51,-52,-56,-59,-64,-65,-66,89,-61,-63,-6,-60,-69,-64,-54,-55,-57,-58,-62,-5,-70,-32,152,]),'CLOSINGPAR':([27,28,37,38,39,40,42,43,44,45,54,62,63,65,68,69,78,86,92,93,97,98,99,113,114,115,116,117,118,119,121,125,134,],[-67,-68,-50,-51,-52,-56,-59,-64,-65,-66,78,-61,-63,-6,99,-60,-69,-64,-54,-55,-57,-58,-62,-74,-75,-76,-77,-78,-79,-80,99,-5,-70,]),'EQUALS':([30,47,51,65,72,104,125,],[56,-45,75,-6,-46,-44,-5,]),'OPENBRACKET':([35,39,47,86,96,100,101,102,103,107,],[59,64,64,64,64,64,-11,-12,-13,64,]),'PLUS':([37,39,40,42,43,44,45,62,63,65,68,69,86,87,92,93,95,96,97,98,99,121,125,],[60,-64,-56,-59,-64,-65,-66,-61,-63,-6,60,-60,-64,60,-54,-55,60,-64,-57,-58,-62,60,-5,]),'MINUS':([37,39,40,42,43,44,45,62,63,65,68,69,86,87,92,93,95,96,97,98,99,121,125,],[61,-64,-56,-59,-64,-65,-66,-61,-63,-6,61,-60,-64,61,-54,-55,61,-64,-57,-58,-62,61,-5,]),'MULTIPLY':([39,40,42,43,44,45,62,63,65,69,86,92,93,96,97,98,99,125,],[-64,66,-59,-64,-65,-66,-61,-63,-6,-60,-64,66,66,-64,-57,-58,-62,-5,]),'DIVIDE':([39,40,42,43,44,45,62,63,65,69,86,92,93,96,97,98,99,125,],[-64,67,-59,-64,-65,-66,-61,-63,-6,-60,-64,67,67,-64,-57,-58,-62,-5,]),'TO':([40,42,43,44,45,62,63,65,69,86,87,92,93,97,98,99,120,125,],[-56,-59,-64,-65,-66,-61,-63,-6,-60,-64,-31,-54,-55,-57,-58,-62,135,-5,]),'COMA':([40,42,43,44,45,47,62,63,65,69,86,92,93,95,96,97,98,99,125,],[-56,-59,-64,-65,-66,71,-61,-63,-6,-60,-64,-54,-55,126,127,-57,-58,-62,-5,]),'CLOSINGBRACKET':([40,42,43,44,45,47,62,63,65,69,72,86,92,93,94,95,96,97,98,99,104,124,125,139,140,],[-56,-59,-64,-65,-66,-45,-61,-63,-6,-60,-46,-64,-54,-55,125,-9,-10,-57,-58,-62,-44,138,-5,-7,-8,]),'AS':([46,47,52,65,72,104,125,],[70,-45,76,-6,-46,-44,-5,]),'INT':([70,76,],[101,101,]),'FLOAT':([70,76,],[102,102,]),'WORD':([70,76,],[103,103,]),'TWOPOINTS':([74,105,],[-16,129,]),'AND':([78,],[110,]),'OR':([78,],[111,]),'NOT':([78,],[112,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'PROGRAMA':([0,],[1,]),'V':([2,],[3,]),'empty':([2,3,7,9,20,32,39,47,77,86,96,100,107,108,122,129,143,151,154,],[5,8,8,23,50,23,65,65,23,65,65,65,65,133,23,8,23,50,23,]),'M':([3,7,129,],[6,21,141,]),'F':([3,7,9,32,77,122,129,143,154,],[7,7,22,58,108,136,7,148,155,]),'E':([3,7,9,32,77,122,129,143,154,],[9,9,9,9,9,9,9,9,9,]),'setType':([4,10,11,59,],[19,24,25,91,]),'endProgram':([6,],[20,]),'EL':([12,18,29,31,41,75,79,80,81,82,83,84,85,90,109,135,],[26,38,38,57,38,38,38,38,38,38,38,38,38,123,134,38,]),'while_first_conditional':([14,15,],[31,32,]),'IDEx':([17,],[34,]),'Ex':([18,29,41,75,79,80,81,82,83,84,85,135,],[36,55,55,106,113,114,115,116,117,118,119,144,]),'EA':([18,29,41,56,64,75,79,80,81,82,83,84,85,88,126,127,135,],[37,37,68,87,95,37,37,37,37,37,37,37,37,121,95,95,37,]),'P':([18,29,41,56,60,61,64,75,79,80,81,82,83,84,85,88,126,127,135,],[40,40,40,40,92,93,40,40,40,40,40,40,40,40,40,40,40,40,40,]),'N':([18,29,41,56,60,61,64,66,67,75,79,80,81,82,83,84,85,88,126,127,135,],[42,42,42,42,42,42,42,97,98,42,42,42,42,42,42,42,42,42,42,42,42,]),'cte':([18,29,41,56,60,61,64,66,67,75,79,80,81,82,83,84,85,88,126,127,135,],[43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,]),'Idv':([19,24,25,71,91,],[46,51,52,104,124,]),'S':([20,151,],[48,153,]),'O':([29,41,],[54,54,]),'saveID':([39,43,86,96,],[62,69,62,62,]),'Arr':([39,47,86,96,100,107,],[63,72,63,63,128,130,]),'first_conditional':([53,],[77,]),'arr_space':([64,126,127,],[94,139,140,]),'T':([70,76,],[100,107,]),'fillSub':([74,],[105,]),'OL':([78,],[109,]),'for_assignation':([87,],[120,]),'while_second_conditional':([89,],[122,]),'Esf':([108,],[131,]),'repeat_conditional':([123,],[137,]),'second_conditional':([132,],[143,]),'final_conditional':([142,],[147,]),'for_conditional':([144,],[149,]),'while_final_conditional':([145,],[150,]),'endProcedure':([146,],[151,]),'for_save_conditional':([152,],[154,]),'for_conditional_end':([157,],[158,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> PROGRAMA","S'",1,None,None,None),
  ('PROGRAMA -> PROGRAM V M endProgram S END','PROGRAMA',6,'p_PROGRAMA','syntax_analyzer.py',365),
  ('endProgram -> <empty>','endProgram',0,'p_endProgram','syntax_analyzer.py',376),
  ('V -> DIM setType Idv AS T Arr','V',6,'p_V','syntax_analyzer.py',383),
  ('V -> empty','V',1,'p_V','syntax_analyzer.py',384),
  ('Arr -> OPENBRACKET arr_space CLOSINGBRACKET','Arr',3,'p_Arr','syntax_analyzer.py',397),
  ('Arr -> empty','Arr',1,'p_Arr','syntax_analyzer.py',398),
  ('arr_space -> EA COMA arr_space','arr_space',3,'p_arr_space','syntax_analyzer.py',406),
  ('arr_space -> ID COMA arr_space','arr_space',3,'p_arr_space','syntax_analyzer.py',407),
  ('arr_space -> EA','arr_space',1,'p_arr_space','syntax_analyzer.py',408),
  ('arr_space -> ID','arr_space',1,'p_arr_space','syntax_analyzer.py',409),
  ('T -> INT','T',1,'p_T','syntax_analyzer.py',418),
  ('T -> FLOAT','T',1,'p_T','syntax_analyzer.py',419),
  ('T -> WORD','T',1,'p_T','syntax_analyzer.py',420),
  ('S -> SUBPROCEDURE ID fillSub TWOPOINTS M RETURN endProcedure S','S',8,'p_S','syntax_analyzer.py',426),
  ('S -> empty','S',1,'p_S','syntax_analyzer.py',427),
  ('fillSub -> <empty>','fillSub',0,'p_fillSub','syntax_analyzer.py',437),
  ('endProcedure -> <empty>','endProcedure',0,'p_endProcedure','syntax_analyzer.py',444),
  ('M -> F M','M',2,'p_M','syntax_analyzer.py',453),
  ('M -> empty','M',1,'p_M','syntax_analyzer.py',454),
  ('F -> E F','F',2,'p_F','syntax_analyzer.py',459),
  ('F -> empty','F',1,'p_F','syntax_analyzer.py',460),
  ('E -> LET setType Idv EQUALS Ex','E',5,'p_E','syntax_analyzer.py',465),
  ('E -> DIM setType Idv AS T Arr','E',6,'p_E','syntax_analyzer.py',466),
  ('E -> IF EL THEN first_conditional F Esf EIF final_conditional','E',8,'p_E','syntax_analyzer.py',467),
  ('E -> FOR ID EQUALS EA for_assignation TO Ex for_conditional DO for_save_conditional F NEXT ID for_conditional_end','E',14,'p_E','syntax_analyzer.py',468),
  ('E -> WHILE while_first_conditional EL DO while_second_conditional F WEND while_final_conditional','E',8,'p_E','syntax_analyzer.py',469),
  ('E -> REPEAT while_first_conditional F UNTIL EL repeat_conditional','E',6,'p_E','syntax_analyzer.py',470),
  ('E -> GOSUB ID','E',2,'p_E','syntax_analyzer.py',471),
  ('E -> INPUT IDEx','E',2,'p_E','syntax_analyzer.py',472),
  ('E -> PRINT Ex','E',2,'p_E','syntax_analyzer.py',473),
  ('for_assignation -> <empty>','for_assignation',0,'p_for_assignation','syntax_analyzer.py',501),
  ('for_conditional -> <empty>','for_conditional',0,'p_for_conditional','syntax_analyzer.py',510),
  ('for_save_conditional -> <empty>','for_save_conditional',0,'p_for_save_conditional','syntax_analyzer.py',521),
  ('for_conditional_end -> <empty>','for_conditional_end',0,'p_for_conditional_end','syntax_analyzer.py',533),
  ('repeat_conditional -> <empty>','repeat_conditional',0,'p_repeat_conditional','syntax_analyzer.py',548),
  ('while_first_conditional -> <empty>','while_first_conditional',0,'p_while_first_conditional','syntax_analyzer.py',558),
  ('while_second_conditional -> <empty>','while_second_conditional',0,'p_while_second_conditional','syntax_analyzer.py',567),
  ('while_final_conditional -> <empty>','while_final_conditional',0,'p_while_final_conditional','syntax_analyzer.py',579),
  ('first_conditional -> <empty>','first_conditional',0,'p_first_conditional','syntax_analyzer.py',592),
  ('second_conditional -> <empty>','second_conditional',0,'p_second_conditional','syntax_analyzer.py',603),
  ('final_conditional -> <empty>','final_conditional',0,'p_final_conditional','syntax_analyzer.py',615),
  ('Esf -> ELSE second_conditional F','Esf',3,'p_Esf','syntax_analyzer.py',623),
  ('Esf -> empty','Esf',1,'p_Esf','syntax_analyzer.py',624),
  ('Idv -> ID COMA Idv','Idv',3,'p_Idv','syntax_analyzer.py',629),
  ('Idv -> ID','Idv',1,'p_Idv','syntax_analyzer.py',630),
  ('Idv -> ID Arr','Idv',2,'p_Idv','syntax_analyzer.py',631),
  ('setType -> <empty>','setType',0,'p_setType','syntax_analyzer.py',646),
  ('IDEx -> ID','IDEx',1,'p_IDEx','syntax_analyzer.py',653),
  ('IDEx -> ID OPENBRACKET setType Idv CLOSINGBRACKET','IDEx',5,'p_IDEx','syntax_analyzer.py',654),
  ('Ex -> EA','Ex',1,'p_Ex','syntax_analyzer.py',660),
  ('Ex -> EL','Ex',1,'p_Ex','syntax_analyzer.py',661),
  ('Ex -> ID','Ex',1,'p_Ex','syntax_analyzer.py',662),
  ('ES -> WORDVAL','ES',1,'p_ES','syntax_analyzer.py',668),
  ('EA -> EA PLUS P','EA',3,'p_EA','syntax_analyzer.py',673),
  ('EA -> EA MINUS P','EA',3,'p_EA','syntax_analyzer.py',674),
  ('EA -> P','EA',1,'p_EA','syntax_analyzer.py',675),
  ('P -> P MULTIPLY N','P',3,'p_P','syntax_analyzer.py',698),
  ('P -> P DIVIDE N','P',3,'p_P','syntax_analyzer.py',699),
  ('P -> N','P',1,'p_P','syntax_analyzer.py',700),
  ('N -> cte saveID','N',2,'p_N','syntax_analyzer.py',724),
  ('N -> ID saveID','N',2,'p_N','syntax_analyzer.py',725),
  ('N -> OPENPAR EA CLOSINGPAR','N',3,'p_N','syntax_analyzer.py',726),
  ('N -> ID Arr','N',2,'p_N','syntax_analyzer.py',727),
  ('saveID -> <empty>','saveID',0,'p_saveID','syntax_analyzer.py',739),
  ('cte -> INTVAL','cte',1,'p_cte','syntax_analyzer.py',759),
  ('cte -> FLOATVAL','cte',1,'p_cte','syntax_analyzer.py',760),
  ('EL -> TRUE','EL',1,'p_EL','syntax_analyzer.py',766),
  ('EL -> FALSE','EL',1,'p_EL','syntax_analyzer.py',767),
  ('EL -> OPENPAR O CLOSINGPAR','EL',3,'p_EL','syntax_analyzer.py',768),
  ('EL -> OPENPAR O CLOSINGPAR OL EL','EL',5,'p_EL','syntax_analyzer.py',769),
  ('OL -> AND','OL',1,'p_OL','syntax_analyzer.py',787),
  ('OL -> OR','OL',1,'p_OL','syntax_analyzer.py',788),
  ('OL -> NOT','OL',1,'p_OL','syntax_analyzer.py',789),
  ('O -> Ex GREATHER Ex','O',3,'p_O','syntax_analyzer.py',797),
  ('O -> Ex GREATHEREQUAL Ex','O',3,'p_O','syntax_analyzer.py',798),
  ('O -> Ex SMALLER Ex','O',3,'p_O','syntax_analyzer.py',799),
  ('O -> Ex SMALLEREQUAL Ex','O',3,'p_O','syntax_analyzer.py',800),
  ('O -> Ex NOTEQUAL Ex','O',3,'p_O','syntax_analyzer.py',801),
  ('O -> Ex EQUALTO Ex','O',3,'p_O','syntax_analyzer.py',802),
  ('O -> Ex error Ex','O',3,'p_O_error','syntax_analyzer.py',829),
  ('empty -> <empty>','empty',0,'p_empty','syntax_analyzer.py',835),
]
