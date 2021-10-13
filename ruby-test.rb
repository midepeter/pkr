#!/usr/bin/env ruby
require 'pg'
DB = PG::Connection.new(dbname: 'pkr', user: 'pkr')

def qa(funk, *params)
  parstr = '(%s)' % (1..params.size).map {|i| "$#{i}"}.join(',')
  r = DB.exec_params("select ok, js from #{funk}#{parstr}", params)[0]
  ok = (r['ok'] == 't')
  js = JSON.parse(r['js'], symbolize_names: true)
  [ok, js]
end

def assert_equal(x1, x2)
  raise 'failed' unless x1 == x2
end

ok, js = qa('things')
assert_equal(true, ok)
assert_equal(2, js.size)
assert_equal('one', js[0][:name])
assert_equal('2021-10-02', js[1][:created_at])

ok, js = qa('thing_get', 999)
assert_equal(false, ok)
assert_equal('not found', js[:error])

ok, js = qa('thing_get', 1)
assert_equal(true, ok)
assert_equal('init', js[:category])
assert_equal(%i(active category created_at id name), js.keys.sort)

ok, js = qa('thing_add', '', 'err')
assert_equal(false, ok)
assert_equal('new row for relation "things" violates check constraint "no_name"', js[:error])

DB.exec("begin")
ok, js = qa('thing_add', 'three', 'test')
assert_equal(true, ok)
assert_equal('three', js[:name])
assert_equal('test', js[:category])
assert_equal(true, js[:active])
DB.exec("rollback")

ok, js = qa('thing_rename', 2, 'one')
assert_equal(false, ok)
assert_equal('duplicate key value violates unique constraint "things_name_key"', js[:error])

DB.exec("begin")
ok, js = qa('thing_rename', 2, 'deux')
assert_equal(true, ok)
assert_equal('deux', js[:name])
assert_equal(false, js[:active])
assert_equal(nil, js[:category])
DB.exec("rollback")

