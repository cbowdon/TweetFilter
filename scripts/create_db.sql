drop table if exists Token;
create table Token (
    access_token    varchar(255) not null,
    token_type      varchar(255) not null
);

drop table if exists User;
create table User (
    id              varchar(255) primary key unique not null,
    name            varchar(255) not null,
    screen_name     varchar(255) not null,
    spammer         boolean
);

drop table if exists Tweet;
create table Tweet (
    id              integer primary key autoincrement,
    text            varchar(140) not null,
    user_id         integer not null,
    spam            boolean,
    foreign key(user_id) references User(id)
);
