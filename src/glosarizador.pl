% glosarizador.pl
% 
% InMemoriAn: Ingenier&iacute;a Lingu&iacute;stica de la Memoria Cultural Andina
% Proyecto FONACIT 2012000099 Venezuela 2013-2016
% Este es un script en Prolog Para generar Glosario del P&aacute;ramo Andino Venezolano
% incorpora las últimas extensiones al código cambiando extrae_una_def
% This is free software with the GNU Affero GPL 3.0 license
% version beta 1
% use: 
% ?- [glosarizador].
% ?- tell('index.html'), genera_glosario, told.
% ?- forall(ambito(A, _, _, _, _), genera_ambito(A)).
% Autor: jacinto@ula.ve
%
:- module(glosarizador, [cargar/2, busca/1, glosariza/1, glosa/0, leer/1, genera_glosario/0]).

:- use_module(library(sgml)).
:- use_module(library(http/html_write)). 
:- use_module(library(readutil)). 
:- encoding(utf8).

% especificación tardía:
% (3)  Cada archivo xml tiene en sus metadatos el ámbito al cual pertenece y que está bajo la etiqueta   <fileGrp ID="">
% Tenemos archivos con las siguientes etiquetas: <fileGrp ID="Miscelánea"> , <fileGrp ID="Salud">, <fileGrp ID="Magia y Religión">, 
% <fileGrp  ID="Gastronomía">, <fileGrp ID="Flora y Fauna">, <fileGrp ID="Costumbres">, <fileGrp ID="Telares y tejidos">, 
% <fileGrp ID="Herramientas y Utensilios">
% La presencia de tales etiquetas dan cuenta de los ámbitos en los que acordamos está segmentado el diccionario de ecocultura. La idea es que, además del diccionario, en la interfaz web la gente tenga la posibilidad de listar los archivos que corresponden a los ámbitos. Esto implica un dibujo para cada ámbito y que la persona le dé click al dibujo para llegar hasta allí. Como en esta fase son solo archivos de audio, video y sus transcripciones, la persona podría acceder a ambos formatos, solo que el formato transcrito tendría que ocultar las etiquetas. ¿Eso es posible?
% Es cierto que este es otro elemento distinto al del diccionario, pero es que tenemos dos niveles: el del corpus (que se llama InMemoriaN) y el del diccionario con las definiciones (que se llama Diccionario de Ecocultura del Páramo Andino de Mérida). Y el del corpus deberíamos mostrarlo también a través de un marco que diga "acceda al Corpus InMemoriaN". Esto de los niveles no lo habíamos comentado antes con estos términos, pero la persona debería tener acceso a ambos.

ambito('Miscelánea', 'miscelanea.png', 'premisc.phtml', 'postmisc.phtml', 'miscelanea.html').
ambito('Salud', 'salud.png', 'presalud.phtml', 'postsalud.phtml', 'salud.html'). 
ambito('Magia y Religión', 'magia_religion.png', 'premagia.phtml', 'postmagia.phtml', 'magia.html'). 
ambito('Gastronomía', 'gastronomia.png', 'pregastro.phtml', 'postgastro.phtml', 'gastronomia.html'). 
ambito('Flora y Fauna', 'flora_y_fauna.png', 'preflora.phtml', 'postflora.phtml', 'florafauna.html'). 
ambito('Costumbres', 'costumbres.png', 'precostu.phtml', 'postcostu.phtml', 'costumbres.html'). 
ambito('Telares y tejidos', 'telares_y_tejidos.png', 'pretela.phtml', 'posttela.phtml', 'telares.html'). 
ambito('Herramientas y Utensilios', 'herramientas_y_utensilios.png', 'preherra.phtml', 'postherra.phtml', 'herramientas.html'). 

% genera el diccionario con las definiciones en el ambito descrito por A. 
genera_ambito(A) :-
  ambito(A, _, _, _, F), tell(F), 
  busca(Definitions, A), 
  print_index_header(A),  
  save_definitions(1, Definitions), 
  print_index_bottom(A), told. 

busca(Ordenadas, A) :-
    directory_files('.', L), 
    select_xml(L, X),
    carga_todos(X, Definiciones, A),
    agrupar_definiciones(Definiciones, Definiciones, Agrupadas), 
    sort(0,  @<, Agrupadas, Ordenadas).

carga_todos([], [], _).
carga_todos([Archivo|RestArc], AllDefs, Ambito) :-
   carga_uno(Archivo, Defs, Ambito), 
   carga_todos(RestArc, RestDefs, Ambito),
   append(Defs, RestDefs, AllDefs).  

carga_uno(Archivo, Defs, A) :- 
   cargar(Archivo, Terminos, A), 
   % nl, write('<p>Archivo: '), write(Archivo), write('</p>'), nl, nl, 
   extrae_definiciones(Terminos, Defs).

%<cesHeader type="Oral" date="2014">
%<fileDesc>
%<fileGrp ID="Costumbres">
%<file ID="AC14CH01054" MIMETYPE="audio/wav" SIZE="8180938" CREATED="2014-01-19" GROUPID="">
%<FLocat LOCTYPE="URL"></FLocat>
%</file>
%</fileGrp>
%<extent><extNote id="Duración"> 00:17:02 </extNote></extent> <sourceDesc>
%<monogr>
%<pubPlace> Chachopo, municipio Miranda </pubPlace>
%</monogr>
%</sourceDesc></fileDesc>
%<group count="2">
%<name type="Entrevistador" id="E"> José Guillermo Gómez </name>
%<name type="Informante" id="I"> Mary Claudia Araujo </name>
%</group>
%</cesHeader>
cargar(Archivo, Doc, Ambito) :- 
   load_structure(Archivo, Term, [dialect(xml)]),
   member(element(doc, _, Doc), Term), 
   member(element(cesHeader, _, Header), Doc),
   member(element(fileDesc,_, Descrip), Header),
   member(element(fileGrp, Param, _), Descrip),
   member('ID'=Ambito, Param).  

% si no es ese ambito, devuelve un Doc vacio
cargar(_, [], _). 
   
% genera el glosario a partir de los .xml que estén en el directorio de trabajo. Todos.
% para ser usada: ?- tell('index.html'), genera_glosario, told. 
genera_glosario :- 
  busca(Definitions), 
  print_index_header,  
  save_definitions(1, Definitions), 
  print_index_bottom. 

listado :-
  busca(D), 
  list_definitions(1, D). 

% busca(-Definiciones) selecciona todos los archivos xml del directorio actual y los procesa con carga_todos
busca(Ordenadas) :-
    directory_files('.', L), 
    select_xml(L, X),
    carga_todos(X, Definiciones),
% falta agrupar y ordenar las definiciones por sus definienda
    agrupar_definiciones(Definiciones, Definiciones, Agrupadas), 
    sort(0,  @<, Agrupadas, Ordenadas).

% para agrupar definiciones
agrupar_definiciones([], _, []).
agrupar_definiciones([(Definiendum, _Definiens, _Params)|R], All, [(Definiendum, TheseDef)|RR]) :-
  findall((Def, Par), member((Definiendum, Def, Par), All), TheseDef),
  remover_duplicados(Definiendum, R, RRR),  
  agrupar_definiciones(RRR, All, RR). 

remover_duplicados(_, [], []). 
remover_duplicados(D, [(D, _Definiens, _Params)|R], RR) :-
  remover_duplicados(D, R, RR). 
remover_duplicados(D, [(D2, _Definiens, _Params)|R], [(D2, _Definiens, _Params)|RR]) :-
  D \= D2, 
  remover_duplicados(D, R, RR).

% save_definitions(Definitions) :- writeq(Definitions). 

% carga_todos(Archivos, Definiciones) acumula las Definiciones de todos los Archivos 
carga_todos([], []).
carga_todos([Archivo|RestArc], AllDefs) :-
   carga_uno(Archivo, Defs), 
   carga_todos(RestArc, RestDefs),
   append(Defs, RestDefs, AllDefs).  

% carga un archivo de definiciones en memoria
carga_uno(Archivo, Defs) :- 
   cargar(Archivo, Terminos), 
   % nl, write('<p>Archivo: '), write(Archivo), write('</p>'), nl, nl, 
   extrae_definiciones(Terminos, Defs).

% cargar(+Archivo, -Contenido) convierte el archivo xml dado en una lista con su contenido como término prolog
cargar(Archivo, Term) :- 
   load_structure(Archivo, Term, [dialect(xml)]).
   % print(Term).

% extrae_definiciones(+Elementos, -Definiens)
extrae_definiciones([], []).
extrae_definiciones(Elementos, ListDefs) :-
   extrae_una_def(Elementos, Definicion, Resto), 
   extrae_partes_def(Definicion, Definiendum, Definiens, Params), 
   ( ( Definiendum \= [] ) -> 
     ( Def = (Definiendum, Definiens, Params), 
       ListDefs = [Def|RestoDefs], 
       extrae_marca(b, Definiens, FlatDefiniens) % No estamos eliminando las negritas
     ) ; ListDefs = RestoDefs ), 
   extrae_definiciones(Resto, RestoDefs). 
extrae_definiciones(_, []). 

% metodo de prueba
% se supone que igual que carga_todos/2 salvo que lo hace sin devolver las definiciones como términos lista
carga_todos_print([]).
carga_todos_print([Archivo|RestArc]) :-
   carga_uno_print(Archivo), 
   carga_todos_print(RestArc).

% metodo de prueba
% carga_uno(Archivo) carga el contenido de un Archivo y le extrae las definiciones por salida estandard
carga_uno_print(Archivo) :-
   cargar(Archivo, Terminos),
   print_work_header(Archivo), 
   nl, write('<p>Archivo: '), write(Archivo), write('</p>'), nl, nl, 
   extrae_def(0, Terminos), % <- Esta es la clave
   extrae_versiones(Terminos), 
   print_work_foot. 

% save_definitions produces html files for every definition, beside the list of 
% terms with the link to the file with its definition. 
save_definitions(_, []). 
save_definitions(N, [(Definiendum, Definiens_P)|RestoDefs]) :-
  % writeq(Definiendum), nl,
  % writeq(Definiens), nl, nl,
  print_def(N, Definiendum, Definiens_P), 
  NN is N + 1, 
  save_definitions(NN, RestoDefs). 

list_definitions(_, []). 
list_definitions(N, [(Definiendum, Definiens_P)|RestoDefs]) :-
  write(N), write('-'), nl, 
  write('Definiendum: '), writeq(Definiendum), nl,
  write_all_definiens(N, Definiens_P), nl, nl, 
  % write('Definiens:' ), writeq(Definiens), nl, 
  % write('Parameters: '), writeq(P), nl, nl,
  NN is N + 1, 
  list_definitions(NN, RestoDefs). 

% select_xml(+Archivos, -ArchivosXML) selecciona los archivos con extensión xml de una lista de archivos dada
select_xml([], []).
select_xml([F|R], [F|RR]) :-
   file_name_extension(_, xml, F),
   select_xml(R, RR).
select_xml([_|R], RR) :-
   % not(file_name_extension(_, xml, F)),
   select_xml(R, RR).

% extrae_marca(+Marca, +Elementos, -Contenidos)
% recorre una lista de elementos y extrae los contenidos bordeados por Marca
% sin atender recursividad 
extrae_marca(_, [], []).
extrae_marca(M, [element(M, _, Contenido)|Rest], [Contenido|RestDefs] ) :-
  extrae_marca(M, Rest, RestDefs).
extrae_marca(M, [element(OtraMarca, _, Contenido)|Rest], AllDefs ) :-
  OtraMarca \= M,
  append(Contenido, Rest, NuevoRest), 
  extrae_marca(M, NuevoRest, AllDefs). 
extrae_marca(M, [_|Rest], RestDefs ) :-
  %write(A), 
  extrae_marca(M, Rest, RestDefs).

% print_def(Definiendum, Definiens) escribe el texto Definiens que define a Definiendum como entrada de diccionario y graba la definición en un archivo al que se referencia. N es el número de la definición en este conjunto y Definiens_Param es una lista de (Definiens, Param). 
print_def(N, Definiendum, Definiens_Param) :-
   Definiendum \= [],
   Definiens_Param \= [],
   nl, nl, 
   % prepara el nombre del archivo de esta definicion
   prepare_def_filename(Definiendum, '.html', Defile), 
   % crea el archivo (o lo sobreescribe si existe) y allí escribe la definicion
   catch( printing_def(N, Defile, Definiendum, Definiens_Param), E, writeln(E) ),    
   % crea una referencia directa a ese archivo desde la salida actual
   % que puede ser el indice del glosario
   print_pre_def,
   write('<h3>'), write(N), write('  <a href="'),
   write(Defile),
   write('">'), write_marks(Definiendum), write('</a></h3>'), nl,
   %  prepare_def_filename(Definiendum, '.jpg', Imagefile),  
   NN is (N mod 16),
   % elige la primer imagen de entre todas las que puedan estar siendo usadas para definir
   Definiens_Param = [ (_, Params)|_], 
   print_image_def(NN, Params),  % imagen asociada
   print_post_def. % cierre definición

% cuando el definiendum o el definiens vienen vacios
print_def(_, Definiendum, Definiens) :-
   ( Definiendum = [] ; Definiens = []),

   write('<h4> Termino incompleto: '),
   write_marks(Definiendum), write('</h4>'), nl,
   write('<h4> Definicion incompleta: '),
   write_marks(Definies), write('</h4>'), nl,
   print_post_def. % cierre definición

printing_def(N, Defile, Definiendum, Definiens_Param) :-   
   open(Defile, write, Stream, [encoding(utf8)]),
   with_output_to(Stream, write_def(N, Defile, Definiendum, Definiens_Param)),  
   close(Stream).

% Genera el nombre del archivo para la definición a partir del definiendum
% que puede ser cualquier atomo. OJO, aún no se verifica la longitud. 
% Ext = '.html' for html files. Other filenames are allowed too. (like .png). 
prepare_def_filename(Definiendum, Ext, Defile) :-
   % aplana el nombre (lo extrae del marcaje)
   % extract_from_marks(Definiendum, '', Def), 
   flat_name(Definiendum, Flat), 
   atomic_list_concat(Flat, Def), 
   % todo en minusculas
   downcase_atom(Def, Name), 
   clean_white(Name, NoWhiteName),
   atom_length(NoWhiteName, Length),  
   ( Length >= 80 -> 
     sub_atom(NoWhiteName, 0, 80, _, FinalName) ;
     FinalName = NoWhiteName ), 
   % con el prefijo inmemorian-def y el sufijo .html
   atomic_concat('inmemorian-def-', FinalName, First),
   atomic_concat(First, Ext, Defile). 

flat_name([], []). 
flat_name([A|R], [A|RR]) :- atom(A), 
   flat_name(R,RR).  
flat_name([element(_, _, C)|R], Name) :-
   append(C,R, NC), 
   flat_name(NC, Name). 

clean_white(Atom, NoWhite) :-
   atom_chars(Atom, Chars),
   remove_white(Chars, None),
   atomic_list_concat(None, NoWhite). 

% elimina los blancos y los simbolos de puntuacion
% para producir un URL sin conflictos
remove_white([], []) :- !. 
remove_white([' '|R], ['-'|RR]) :- !, 
  remove_white(R, RR). 
remove_white(['\n'|R], RR) :- !,
  remove_white(R, RR). 
remove_white(['\r'|R], RR) :- !,
  remove_white(R, RR). 
remove_white([','|R], RR) :- !,
  remove_white(R, RR). 
remove_white(['.'|R], RR) :- !,
  remove_white(R, RR). 
remove_white(['?'|R], RR) :- !,
  remove_white(R, RR).
remove_white(['¿'|R], RR) :- !,
  remove_white(R, RR). 
remove_white(['!'|R], RR) :- !,
  remove_white(R, RR).
remove_white(['¡'|R], RR) :- !,
  remove_white(R, RR). 
remove_white([':'|R], RR) :- !,
  remove_white(R, RR). 
remove_white([';'|R], RR) :- !,
  remove_white(R, RR).  
remove_white([H|R], [H|RR]) :- !,
  remove_white(R, RR).

% changing write_def to allow for multiple definiens for the same definiendum
write_def(N, Defile, Definiendum, Definiens_Param) :-
   flat_name(Definiendum, FlatDef), 
   atomic_list_concat(FlatDef, OneAtomDef), 
   print_header_def(OneAtomDef), % pre-encabezado
   print_header, % encabezado
   print_pre_def, % previo a definición
   write('<h3><defdum>'),write_marks(Definiendum), write('</defdum></h3>'), nl,
   write('<table style="width:100%">'), 
   write('<tr> <th>Medio</th> <th>Textual</th> </tr>'), 
   write_all_definiens(N, Definiens_Param), 
   write('</table> '), 
   print_post_def, % cierre definición
   print_bottom.  % pie de ese archivo

write_all_definiens(_,[]). 
write_all_definiens(N, [(Definiens, Param)|RestD]) :-  
   NN is (N mod 16),  
   write('<tr><td>'), 
   print_image_def(NN, Param),   % imagen o media asociada
   write('</td> <td>'), 
   write('<b>Definici&oacute;n:</b><p>'),
   write_marks(Definiens), write('</p>'),
   write('</td></tr>'), 
   write_all_definiens(NN, RestD). 

print_header_def(Definiendum) :-
    write('<!doctype html>'), nl, 
    write('<html lang="es">'), nl, 
    write('<head>'), nl, 
    write('<meta charset="UTF-8">'), nl, 
    write('<title>InMemoriAn: '), 
    write(Definiendum),
    write('</title>'), nl, nl. 

% with predefined image
print_image_def(_Imagen, Params) :-
   member(file=Filename, Params),
   file_is_image(Filename),  
   (member(source=S, Params) -> Source=S; Source="fuente no identificada"), 
   write('<a class="photo_hover3" href="#">'), 
   write('	<img src="media/'), write(Filename), 
   write('" alt="Imagen tomada de: '), write(Source), write('" width="480" height="428" /></a>'). 	

% with predefined audio (for the time being using this confusing name)
print_image_def(_Imagen, Params) :-
   member(file=Filename, Params), 
   file_is_audio_wav(Filename), nl, nl, write('audio en '), write(Filename), nl, nl,  !, 
   write('<audio controls> <source src="media/'), write(Filename), write('" type="audio/wav">Your browser does not support the audio element.</audio>'). 

% without image
print_image_def(Imagen, _Params) :-
   write('<a class="photo_hover3" href="#">'), 
   write('	<img src="media/'), write(Imagen), write('.jpg" alt="Imagen " width="480" height="428" /></a>'). 	

file_is_image(Name) :- 
   (sub_atom(Name, _, _, 0, 'jpg'); sub_atom(Name, _, _, 0, 'jpeg');
    sub_atom(Name, _, _, 0, 'png'); sub_atom(Name, _, _, 0, 'gif')). 

file_is_audio_wav(Name) :- 
   sub_atom(Name, _, _, 0, 'wav').

file_is_audio_mp3(Name) :- 
   sub_atom(Name, _, _, 0, 'mp3'). 		 

print_pre_def :-
   write('<section class="group1"> <p>'), nl. 

print_post_def :-
   write('</p>'), nl,
   % write('<a href="#"><span class="button">Definici&oacute;n</span></a>'), nl, % no requiere otro boton
   write('</section>'). 

print_header :- 
  % copy above.phtml to current output
  leer('above.phtml'). 

print_bottom :-
  % copy below.phtml to current output
  leer('below.phtml'). 

print_index_header :- 
  % copy preindex.phtml to current output
  leer('preindex.phtml'). 

print_index_bottom :- 
  % copy postindex.phtml to current output
  leer('postindex.phtml'). 

print_index_header(A) :- 
  % copy pre-A-.phtml to current output
  ambito(A, _, F, _,_), 
  leer(F). 

print_index_bottom(A) :- 
  % copy post-A-.phtml to current output
  ambito(A, _, _, F,_), 
  leer(F). 

% reescribe la lista de elements como un texto con marcas xml
write_marks([]).
write_marks([element(M, _, Content)|Rest]) :-
   write('<'), write(M), write('>'), 
   write_marks(Content), nl, 
   write('</'), write(M), write('>'),
   write_marks(Rest).
write_marks([Atom|Rest]) :-
   atom(Atom), 
   write(Atom),
   write_marks(Rest).
write_marks([S|R]) :- 
   write_marks(S), write_marks(R). 

% extract_from_marks
extract_from_marks([], Atom, Atom).
extract_from_marks([element(M, _, Content)|Rest], Prev, Final) :-
   % write('<'), write(M), write('>'), 
   extract_from_marks(Content, Prev, Interm), nl, 
   % write('</'), write(M), write('>'),
   atomic_concat(Previo, Interm, Siguiente),
   extract_from_marks(Rest, Siguiente, Final).
extract_from_marks([Atom|Rest], Previo, Final) :-
   atom(Atom), 
   % write(Atom),
   atomic_concat(Previo, Atom, Interm),
   extract_from_marks(Rest, Interm, Final).
extract_from_marks([S|R], P, F) :- 
   extract_from_marks(S, P, I), 
   extract_from_marks(R, I, F). 

% al parecer la estructura obliga
% Resto contiene al resto de los elementos, incluyendo el contenido de esta definición que podría tener otras definiciones
% extrae_una_def(+Elementos, -Definicion, -Resto)
extrae_una_def([], [], []).
extrae_una_def([element(def, _, Contenido)|Rest], Contenido, RRest ) :- 
   append(Contenido, Rest, RRest). 
extrae_una_def([element(OtraMarca, _, Contenido)|Rest], Def, Resto ) :-
   OtraMarca \= def,
   append(Contenido, Rest, NuevoRest), 
   extrae_una_def(NuevoRest, Def, Resto). 
extrae_una_def([A|Rest], Def, RR ) :-
   atom(A),  
   extrae_una_def(Rest, Def, RR).

% extrae_def(+Contador, +Elementos)
extrae_def(_, []).
extrae_def(C, Elementos):-
   extrae_una_def(Elementos, Def, Resto), 
   extrae_partes_def(Def, Definiendum, Definiens, Param),
   print_def(C, Definiendum, Definiens, Param), CC is (C + 1) mod 16,  
   extrae_def(CC, Resto). 
extrae_def(_, _). 

% extrae_partes_def(+Contenido, -Definiendum, -Definiens, -Parameters)
extrae_partes_def(Def, Definiendum, Definiens, Param) :- 
   extrae_partes_def(Def, [], Defdum, OutDefdum, Definiens, Param),
   (Defdum=[] -> Definiendum = OutDefdum ; Definiendum = Defdum). 

% extrae_partes_def(+Contenido, InDefdum, Defdum, OutDefdum, Definiens, Parametros)
extrae_partes_def([], Out,  [], Out, [], []).  
extrae_partes_def([element(ldefiniens, LP, LDefiniens)|Rest], In, Defdum, Out, Definiens, P) :-
   extrae_partes_def(Rest, In, Defdum, Out, RDefiniens, RP), !, 
   append(LDefiniens, ['...'], LDefiniensDots), 
   append(LDefiniensDots, RDefiniens, Definiens),
   append(LP, RP, P).
extrae_partes_def([element(rdefiniens, P, Definiens)|Rest], In, Defdum, Out, Definiens, P ) :-
   extrae_partes_def(Rest, In, Defdum, Out, _, _). 
extrae_partes_def([element(defdum, _, Defdum)|Rest], In, Defdum, Out, Definiens, P) :-
   extrae_partes_def(Rest, In, _, Out, Definiens, P). 
extrae_partes_def([element(OtraMarca, _, Contenido)|Rest], InD, Defdum, OutD, Definiens, P ) :-
   OtraMarca \= rdefiniens,
   OtraMarca \= ldefiniens,
   OtraMarca \= defdum, 
   append(Contenido, Rest, NuevoRest), 
   extrae_partes_def(NuevoRest, InD, Defdum, OutD, Definiens, P).  
extrae_partes_def([A|R], InD, Defdum, OutD, Definiens, P) :- 
   atom(A),  
   %write('-{'), write(A), write('}-'), 
   append(InD, [A], NextD), 
   extrae_partes_def(R, NextD, Defdum, OutD, Definiens, P).


% y si es una lista, la achata.. 
%extrae_partes_def([Contenido|Rest], InD, OutD, Definiens ) :-
%         append(Contenido, Rest, NuevoRest), 
%         extrae_partes_def(NuevoRest, InD, OutD, Definiens).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Auxiliaries

glosa :- 
    %write('Glosarizando los siguientes archivos'), nl, 
    directory_files('.', L), 
    select_xml(L, X),
    print_header, 
    carga_todos_print(X), 
    print_bottom. 
  

% ?- directory_files('.', L).
% L = ['Z08MX01002_3.xml', 'Z07MX01005_5.xml', 'Z08MX01003_4.xml', 'Z08MX09001_3.xml', 'Z08MX02001_3.xml', .., '.',  'F07MX01004_3.xml', 'glosarizador.pl'|...].

% ?- file_name_extension(B, xml, 'Z08MX01002_3.xml'). 
% B = 'Z08MX01002_3'.

% experimento de lectura con codificación
%

% abrir archivo en stream, leerlo y mostrarlo en pantalla

%octet
%    Default  encoding for binary streams.  This causes the stream  to be
%    read and written fully untranslated.

%ascii
%    7-bit  encoding  in 8-bit  bytes.   Equivalent  to  iso_latin_1,  but
%    generates errors and warnings on encountering values above 127.

%iso_latin_1
%    8-bit  encoding supporting many western languages.  This  causes the
%    stream to be read and written fully untranslated.

%text
%    C-library  default locale encoding for text  files.  Files are  read
%    and  written using the C-library functions mbrtowc()  and wcrtomb().
%    This may be the  same as one of the other locales, notably it may be
%    the  same as iso_latin_1 for western languages  and utf8 in a  UTF-8
%    context.

%utf8
%    Multi-byte encoding of full UCS, compatible with ascii.  See above.

%unicode_be
%    Unicode  Big  Endian.     Reads  input  in  pairs  of  bytes,   most
%    significant byte first.  Can only represent 16-bit characters.

%unicode_le
%    Unicode  Little  Endian.    Reads input  in  pairs of  bytes,  least
%    significant byte first.  Can only represent 16-bit characters.


leer(Archivo) :-
    open(Archivo, read, Fd, [encoding(utf8)]),
    lee_escribe(Fd),
    close(Fd).  

lee_escribe(Entrada) :- at_end_of_stream(Entrada).
lee_escribe(Entrada) :- get_code(Entrada, Code), put_code(Code), lee_escribe(Entrada). 


% extrae_versiones(Contenido) separa la sección de versiones de un archivo y la
% reporta en pantalla 
extrae_versiones(T) :-
  extrae_marca(versiones, T, Versiones), 
  write_marks(Versiones). 

% separar_rdefiniens(+ListaDefiniciones, -Definiendum, -Definiens)
% supone que no hay definiciones anidadas
separar_rdefiniens([], [], []).
separar_rdefiniens([element(rdefiniens, _, Definiens)|Rest], Rest, Definiens).
separar_rdefiniens([element(OtraMarca, _, Contenido)|Rest], Definiendum, Definiens ) :-
	OtraMarca \= rdefiniens,
         append(Contenido, Rest, NuevoRest), 
         separar_rdefiniens(NuevoRest, Definiendum, Definiens).  
separar_rdefiniens([A|R], [A|RR], Definiens) :- 
         atom(A),  
	separar_rdefiniens(R, RR, Definiens).
% y si es una lista, la achata.. 
separar_rdefiniens([Contenido|Rest], Definiendum, Definiens ) :-
         append(Contenido, Rest, NuevoRest), 
         separar_rdefiniens(NuevoRest, Definiendum, Definiens).

% glosariza(Lista_de_Definiciones) procesa la lista y separa la definiciones que encuentre, imprimiendo cada una
glosariza([]).
glosariza([Def|RDefs]) :-
   Def \= [], 
   separar_rdefiniens(Def, Definiendum, Definiens), 
   print_def(Definiendum, Definiens), 
   glosariza(RDefs).
glosariza([_|R]) :- glosariza(R).  



% fin del archivo

