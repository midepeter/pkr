package main

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

var thing Thing

func TestGetAllThings(t *testing.T) {
	ok, js := qa("things")
	things, _ := MarshalIntoMap(js)
	assert.Equal(t, true, ok)
	assert.Equal(t, 2, len(things))
	assert.Equal(t, "one", things[0]["name"])
	assert.Equal(t, "2021-10-02", things[1]["created_at"])
}

func TestGetThingFromDbWithId(t *testing.T) {
	ok, js := qa("thing_get", 1)
	err := json.Unmarshal(js, &thing)
	assert.Equal(t, true, ok)
	assert.NoError(t, err)
	assert.Equal(t, "init", thing["category"])
}

func TestGetThingFromDbWithWrongId(t *testing.T) {
	ok, js := qa("thing_get", 999)
	err := json.Unmarshal(js, &thing)
	assert.Equal(t, false, ok)
	assert.NoError(t, err)
	assert.Equal(t, "error", "error")
}

func TestThingAddWithEmptyInput(t *testing.T) {
	ok, js := qa("thing_add", "", "err")
	err := json.Unmarshal(js, &thing)
	assert.Equal(t, false, ok)
	assert.NoError(t, err)
	assert.Equal(t, `new row for relation "things" violates check constraint "no_name"`, thing["error"])
}

//Implementing db transactions for rollbacks after thing_add() and thing_rename() tests
func qaTest(db *sql.Tx, funk string, args ...interface{}) (bool, []byte) {
	var (
		ok     sql.NullBool
		js     []byte
		params []string
	)
	for i := range args {
		params = append(params, fmt.Sprintf("$%d", i+1))
	}
	err := db.QueryRow(fmt.Sprintf(`SELECT ok, js FROM %s(%s)`, funk, strings.Join(params, ",")), args...).Scan(&ok, &js)
	if err != nil {
		return false, nil
	}
	return ok.Bool, js
}

func TestThingAddWithRightInput(t *testing.T) {
	tx, _ := db.Begin()
	ok, js := qaTest(tx, "thing_add", "six", "prod")
	err := json.Unmarshal(js, &thing)
	assert.Equal(t, true, ok)
	assert.NoError(t, err)
	assert.Equal(t, "six", thing["name"])
	assert.Equal(t, "prod", thing["category"])
	tx.Rollback()
}

func TestThingRenameExistingInput(t *testing.T) {
	tx, _ := db.Begin()
	ok, js := qaTest(tx, "thing_rename", 2, "deux")
	err := json.Unmarshal(js, &thing)
	assert.Equal(t, true, ok)
	assert.NoError(t, err)
	assert.Equal(t, "deux", thing["name"])
	assert.Equal(t, false, thing["active"])
	assert.Equal(t, nil, thing["category"])
	tx.Rollback()
}
