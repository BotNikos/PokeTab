pragma foreign_keys = on;
drop table if exists items;
drop table if exists categories;
drop table if exists history;
drop table if exists query_types;

create table categories (
	   id integer primary key,
	   title text,
	   color text
);

create table items (
	   id integer primary key,
	   title text,
	   url text,
	   category_id integer references categories (id) on delete cascade on update cascade
);

create table query_types (
	   id integer primary key,
	   title text
);

create table history (
	   id integer primary key,
	   query_type_id integer references query_types (id) on delete cascade on update cascade,
	   query blob,
	   time text default (datetime ('now', 'localtime'))
);
