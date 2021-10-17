package main

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"log"
	"strings"

	_ "github.com/lib/pq"
)

type Thing map[string]interface{}
type Things []Thing

func main() {
	//Example
	var thing Thing
	_, js := qa("things")
	things, err := MarshalIntoMap(js)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(things[0]["name"])

	_, js = qa("thing_get", 2)
	err = json.Unmarshal(js, &thing)
	if err != nil {
		log.Println("Unable to serialize", err)
	}
	fmt.Println(thing["name"])
}

func conn() *sql.DB {
	dburl := "postgres://pkr:pkr@localhost:5432/pkr"
	db, err := sql.Open("postgres", dburl)
	if err != nil {
		log.Printf("Unable to connect to db %s", err)
	}
	return db
}

var db = conn()

func qa(funk string, args ...interface{}) (bool, []byte) {
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

func MarshalIntoMap(js []byte) (Things, error) {
	var things Things
	err := json.Unmarshal(js, &things)
	if err != nil {
		fmt.Println(err)
	}
	return things, nil
}
