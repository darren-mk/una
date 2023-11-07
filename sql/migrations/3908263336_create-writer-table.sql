create extension if not exists
    "uuid-ossp";
create table writer (
    id uuid default uuid_generate_v4(),
    first_name text not null,
    primary key (id));
insert into migrations (timing, description)
    values (3908263336, 'create writer table');
