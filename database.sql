pragma foreign_keys = on;
drop table if exists items;
drop table if exists categories;
drop table if exists history;
drop table if exists query_types;
drop table if exists themes;
drop table if exists theme_colors;
drop table if exists colors;

create table categories (
	id integer primary key,
	title text,
	color_num integer
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
	id			integer primary key,
	title		text,
	selected	integer
);

create table theme_colors (
	id			integer primary key,
	theme_id	integer,
	color_num	integer,
	color		text
);

insert into themes ( title, selected ) values ( 'Dracula', 1 );
insert into themes ( title, selected ) values ( 'Monokai', 0 );
insert into themes ( title, selected ) values ( 'Catppuccin', 0 );
insert into themes ( title, selected ) values ( 'Gruvbox', 0 );
insert into themes ( title, selected ) values ( 'Nord', 0 );
insert into themes ( title, selected ) values ( 'Everforest', 0 );

-- Dracula theme_colors
insert into theme_colors ( theme_id, color_num, color ) values ( 1, 1, '#282A36' ); -- bg color
insert into theme_colors ( theme_id, color_num, color ) values ( 1, 2, '#44475A' ); -- bg2 color
insert into theme_colors ( theme_id, color_num, color ) values ( 1, 3, '#F8F8F2' ); -- fg color
insert into theme_colors ( theme_id, color_num, color ) values ( 1, 4, '#6272A4' ); -- fg2 color
insert into theme_colors ( theme_id, color_num, color ) values ( 1, 5, '#BD93F9' ); -- accient color
insert into theme_colors ( theme_id, color_num, color ) values ( 1, 6, '#FF5555' ); -- error color
insert into theme_colors ( theme_id, color_num, color ) values ( 1, 7, '#F1FA8C' ); -- warning color
insert into theme_colors ( theme_id, color_num, color ) values ( 1, 8, '#50FA7B' ); -- success color

-- Monokai theme_colors
insert into theme_colors ( theme_id, color_num, color ) values ( 2, 1, '#2e2e2e' ); -- bg color
insert into theme_colors ( theme_id, color_num, color ) values ( 2, 2, '#797979' ); -- bg2 color
insert into theme_colors ( theme_id, color_num, color ) values ( 2, 3, '#d6d6d6' ); -- fg color
insert into theme_colors ( theme_id, color_num, color ) values ( 2, 4, '#6272A4' ); -- fg2 color
insert into theme_colors ( theme_id, color_num, color ) values ( 2, 5, '#9e86c8' ); -- accient color
insert into theme_colors ( theme_id, color_num, color ) values ( 2, 6, '#b05279' ); -- error color
insert into theme_colors ( theme_id, color_num, color ) values ( 2, 7, '#e5b567' ); -- warning color
insert into theme_colors ( theme_id, color_num, color ) values ( 2, 8, '#b4d273' ); -- success color

-- Catppuccin theme_colors
insert into theme_colors ( theme_id, color_num, color ) values ( 3, 1, '#1e1e2e' ); -- bg color
insert into theme_colors ( theme_id, color_num, color ) values ( 3, 2, '#45475a' ); -- bg2 color
insert into theme_colors ( theme_id, color_num, color ) values ( 3, 3, '#cdd6f4' ); -- fg color
insert into theme_colors ( theme_id, color_num, color ) values ( 3, 4, '#a6adc8' ); -- fg2 color
insert into theme_colors ( theme_id, color_num, color ) values ( 3, 5, '#cba6f7' ); -- accient color
insert into theme_colors ( theme_id, color_num, color ) values ( 3, 6, '#f38ba8' ); -- error color
insert into theme_colors ( theme_id, color_num, color ) values ( 3, 7, '#f9e2af' ); -- warning color
insert into theme_colors ( theme_id, color_num, color ) values ( 3, 8, '#a6e3a1' ); -- success color

-- Gruvbox theme_colors
insert into theme_colors ( theme_id, color_num, color ) values ( 4, 1, '#282828' ); -- bg color
insert into theme_colors ( theme_id, color_num, color ) values ( 4, 2, '#3c3836' ); -- bg2 color
insert into theme_colors ( theme_id, color_num, color ) values ( 4, 3, '#ebdbb2' ); -- fg color
insert into theme_colors ( theme_id, color_num, color ) values ( 4, 4, '#d5c4a1' ); -- fg2 color
insert into theme_colors ( theme_id, color_num, color ) values ( 4, 5, '#b8bb26' ); -- accient color
insert into theme_colors ( theme_id, color_num, color ) values ( 4, 6, '#cc241d' ); -- error color
insert into theme_colors ( theme_id, color_num, color ) values ( 4, 7, '#d79921' ); -- warning color
insert into theme_colors ( theme_id, color_num, color ) values ( 4, 8, '#689d6a' ); -- success color

-- Gruvbox theme_colors
insert into theme_colors ( theme_id, color_num, color ) values ( 5, 1, '#2e3440' ); -- bg color
insert into theme_colors ( theme_id, color_num, color ) values ( 5, 2, '#4c566a' ); -- bg2 color
insert into theme_colors ( theme_id, color_num, color ) values ( 5, 3, '#d8dee9' ); -- fg color
insert into theme_colors ( theme_id, color_num, color ) values ( 5, 4, '#eceff4' ); -- fg2 color
insert into theme_colors ( theme_id, color_num, color ) values ( 5, 5, '#88c0d0' ); -- accient color
insert into theme_colors ( theme_id, color_num, color ) values ( 5, 6, '#bf616a' ); -- error color
insert into theme_colors ( theme_id, color_num, color ) values ( 5, 7, '#ebcb8b' ); -- warning color
insert into theme_colors ( theme_id, color_num, color ) values ( 5, 8, '#a3be8c' ); -- success color

-- Everforest theme colors
insert into theme_colors ( theme_id, color_num, color ) values ( 6, 1, '#272E33' ); -- bg color
insert into theme_colors ( theme_id, color_num, color ) values ( 6, 2, '#2E383C' ); -- bg2 color
insert into theme_colors ( theme_id, color_num, color ) values ( 6, 3, '#D3C6AA' ); -- fg color
insert into theme_colors ( theme_id, color_num, color ) values ( 6, 4, '#859289' ); -- fg2 color
insert into theme_colors ( theme_id, color_num, color ) values ( 6, 5, '#A7C080' ); -- accient color
insert into theme_colors ( theme_id, color_num, color ) values ( 6, 6, '#E67E80' ); -- error color
insert into theme_colors ( theme_id, color_num, color ) values ( 6, 7, '#DBBC7F' ); -- warning color
insert into theme_colors ( theme_id, color_num, color ) values ( 6, 8, '#83C092' ); -- success color


