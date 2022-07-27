#!/bin/bash -ex

sqlite3 carte.db 'create table meals (name text);'
sqlite3 carte.db 'insert into meals values("Nudeln mit Tomatensoße");'


sqlite3 carte.db 'create table user_tokens (token text);'
sqlite3 carte.db 'insert into user_tokens values("8bc10b64-f861-4d20-8b96-30438e863277");'


# sqlite3 carte.db 'create table cartes (user_token_id integer, meal_id integer, foreign key(user_token_id) references user_tokens(ROW_ID), foreign key(meal_id) references meals(ROW_ID));'
sqlite3 carte.db << EOM
    create table cartes (
        user_token_id integer, 
        meal_id integer, 
        foreign key(user_token_id) references user_tokens(ROWID), 
        foreign key(meal_id) references meals(ROWID)
    );
EOM
sqlite3 carte.db 'insert into cartes values(1,1);'
