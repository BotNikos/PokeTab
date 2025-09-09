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

create table themes (
	id		integer primary key,
	title		text,
	selected	int,
	accent		text,
	bg		text,
	bg2		text,
	fg		text,
	fg2		text,
	error		text,
	warning	text,
	success	text
);

insert into themes (title, selected, accent, bg, bg2, fg, fg2, error, warning, success) values (
'Dracula', 1, '#BD93F9', '#282A36', '#44475A', '#F8F8F2', '#6272A4', '#FF5555', '#F1FA8C', '#50FA7B');
