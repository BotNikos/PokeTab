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

insert into categories (title, color) values
('Games', '#8BE9FD'),
('Work', '#FFB86C'),
('Books', '#50FA7B');

insert into items (title, url, category_id) values
-- Games
('StopGame', 'https://stopgame.ru', 1),
('DFT', 'https://dtf.ru/games', 1),
('Metacritic', 'https://metacritic.com', 1),
('Psprices', 'https://psprices.com/region-us/index', 1),
('Eshop usa', 'https://www.nintendo.com/us/store/', 1),
-- Work
('192.168.4.16', 'http://192.168.4.16', 2),
('192.168.4.116', 'http://192.168.4.116', 2),
('GitLab', 'http://172.16.80.2/', 2),
('YouTrack', 'http://172.16.80.1/issues', 2),
-- Books
('zLibrary', 'https://z-lib.id/', 3);

insert into query_types (title) values
('Create category'),
('Create item'),
('Goto item'),
('Search');
