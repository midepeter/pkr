package main

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"log"
	"strings"

	_ "github.com/lib/pq"
)

func main() {
	//Url to connect to database
	dburl := "postgres://pkr:pkr@localhost:5432/pkr"
	db, err := sql.Open("postgres", dburl)
	if err != nil {
		log.Printf("Unable to connect to db %s", err)
	}
	_, err = MarshaIntoMap(db)
	if err != nil {
		fmt.Println(err)
	}
}

func qa(db *sql.DB, funk string, args ...interface{}) (bool, []byte) {
	var (
		ok     sql.NullBool
		js     []byte
		params []string
	)
	for i, _ := range args {
		params = append(params, fmt.Sprintf("$%d", i+1))
	}

	err := db.QueryRow(fmt.Sprintf(`SELECT ok, js FROM %s(%s)`, funk, strings.Join(params, ",")), args...).Scan(&ok, &js)
	if err != nil {
		return false, nil
	}

	return ok.Bool, js
}

func MarshaIntoMap(db *sql.DB) (interface{}, error) {
	var things []map[string]interface{}

	ok, js := qa(db, "things")
	err := json.Unmarshal([]byte(js), &things)
	if err != nil {
		return nil, err
	}
	fmt.Println(ok)
	fmt.Println(things[0]["name"])
	return things, nil
}
