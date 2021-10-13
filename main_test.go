package main

import (
	"encoding/json"
	"testing"

	"github.com/stretchr/testify/assert"
)

var (
	things Things
	thing  Thing
)

func Test_main(t *testing.T) {
	db, err := connectDb()
	if err != nil {
		panic(err)
	}
	if err := db.Ping(); err != nil {
		panic(err)
	}
	defer db.Close()

	ok, js := qa(db, "things")

	err = json.Unmarshal(js, &things)
	assert.Equal(t, true, ok)
	assert.NoError(t, err)
	assert.Equal(t, 2, len(things))
	assert.Equal(t, "one", things[0].String("name"))
	assert.Equal(t, "2021-10-02", things[1].String("created_at"))

	ok, js = qa(db, "thing_get", 999)
	err = json.Unmarshal(js, &thing)
	assert.Equal(t, false, ok)
	assert.NoError(t, err)
	assert.Equal(t, "not found", thing.String("error"))

	ok, js = qa(db, "thing_get", 1)
	err = json.Unmarshal(js, &thing)
	assert.Equal(t, true, ok)
	assert.NoError(t, err)
	assert.Equal(t, "init", thing.String("category"))
	//there is no sort function like this in go. you need create custom sort key function
	//assert_equal(%i(active category created_at id name), js.keys.sort)

	ok, js = qa(db, "thing_add", "", "err")
	err = json.Unmarshal(js, &thing)
	assert.Equal(t, false, ok)
	assert.NoError(t, err)
	assert.Equal(t, `new row for relation "things" violates check constraint "no_name"`, thing.String("error"))

	tx, _ := db.Begin()
	ok, js = qa(tx, "thing_add", "three", "test")
	err = json.Unmarshal(js, &thing)
	assert.Equal(t, true, ok)
	assert.NoError(t, err)
	assert.Equal(t, "three", thing.String("name"))
	assert.Equal(t, "test", thing.String("category"))
	tx.Rollback()

	ok, js = qa(db, "thing_rename", 2, "one")
	assert.Equal(t, false, ok)
	err = json.Unmarshal(js, &thing)
	assert.NoError(t, err)
	assert.Equal(t, `duplicate key value violates unique constraint "things_name_key"`, thing.String("error"))

	tx, _ = db.Begin()
	ok, js = qa(tx, "thing_rename", 2, "deux")
	err = json.Unmarshal(js, &thing)
	assert.Equal(t, true, ok)
	assert.NoError(t, err)
	assert.Equal(t, "deux", thing.String("name"))
	assert.Equal(t, false, thing.Bool("active"))
	//we cant use nil for category. it should empty string
	assert.Equal(t, "", thing.String("category"))

	tx.Rollback()
}
