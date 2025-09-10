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

insert into themes (title, selected, accent, bg, bg2, fg, fg2, error, warning, success) values ('Dracula', 0, '#BD93F9', '#282A36', '#44475A', '#F8F8F2', '#6272A4', '#FF5555', '#F1FA8C', '#50FA7B');
insert into themes (title, selected, accent, bg, bg2, fg, fg2, error, warning, success) values ('Monokai', 0, '#9e86c8', '#2e2e2e', '#797979', '#d6d6d6', '#6272A4', '#b05279', '#e5b567', '#b4d273');
insert into themes (title, selected, accent, bg, bg2, fg, fg2, error, warning, success) values ('Catppuccin', 0, '#cba6f7', '#1e1e2e', '#45475a', '#cdd6f4', '#a6adc8', '#f38ba8', '#f9e2af', '#a6e3a1');
insert into themes (title, selected, accent, bg, bg2, fg, fg2, error, warning, success) values ('Gruvbox', 0, '#b8bb26', '#282828', '#3c3836', '#ebdbb2', '#d5c4a1', '#cc241d', '#d79921', '#689d6a');
insert into themes (title, selected, accent, bg, bg2, fg, fg2, error, warning, success) values ('Nord', 0, '#88c0d0', '#2e3440', '#4c566a', '#d8dee9', '#eceff4', '#bf616a', '#ebcb8b', '#a3be8c');
