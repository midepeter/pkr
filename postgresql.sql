create table things (
	id serial primary key,
	created_at date not null default current_date,
	active boolean not null default true,
	name text not null unique constraint no_name check (length(name) > 1),
	category text
);

-- test data
insert into things (created_at, name, category) values ('2021-10-01', 'one', 'init');
insert into things (created_at, name, active) values ('2021-10-02', 'two', false);

-- API: list all things
create function things(
	out ok boolean, out js jsonb) as $$
begin
	js := json_agg(r) from (
		select id, created_at, active, name, category
		from things
		order by id
	) r;
	ok := true;
end;
$$ language plpgsql;

-- API: get one by id
create function thing_get(integer,
	out ok boolean, out js jsonb) as $$
begin
	js := row_to_json(r) from (
		select id, created_at, active, name, category
		from things
		where id = $1
	) r;
	if js is null then
		js := jsonb_build_object('error', 'not found');
	else
		ok := true;
	end if;
end;
$$ language plpgsql;

-- API: create with name, category
create function thing_add(text, text,
	out ok boolean, out js jsonb) as $$
declare
	err text;
begin
	with nu as (
		insert into things (name, category)
		values ($1, $2)
		returning id, created_at, active, name, category
	)
	select to_jsonb(nu.*) into js from nu;
	ok := true;
exception
	when others then get stacked diagnostics err = message_text;
	js := jsonb_build_object('error', err);
	ok := false;
end;
$$ language plpgsql;

-- API: give id and new name to update name
create function thing_rename(integer, text,
	out ok boolean, out js jsonb) as $$
declare
	err text;
begin
	with nu as (
		update things
		set name = $2
		where id = $1
		returning id, created_at, active, name, category
	)
	select to_jsonb(nu.*) into js from nu;
	ok := true;
exception
	when others then get stacked diagnostics err = message_text;
	js := jsonb_build_object('error', err);
	ok := false;
end;
$$ language plpgsql;

