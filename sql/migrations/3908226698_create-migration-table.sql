create table migrations (
    timing bigint primary key,
    description text not null);
insert into migrations (timing, description)
    values (3908226698, 'create migration table');
