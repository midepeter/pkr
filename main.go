package main

import (
	"database/sql"
	"fmt"
	"log"
	"strings"

	_ "github.com/lib/pq"
)

const (
	dbHost     = "localhost"
	dbPort     = "5432"
	dbUser     = "pkr"
	dbPassword = "pkr"
	dbName     = "pkr"
)

type Thing map[string]interface{}

func (t Thing) String(key string) string {
	v, ok := t[key]
	if !ok {
		return ""
	}

	s, ok := v.(string)
	if !ok {
		return ""
	}

	return s
}

func (t Thing) Bool(key string) bool {
	v, ok := t[key]
	if !ok {
		return false
	}
	b, ok := v.(bool)
	if !ok {
		return false
	}
	return b
}

type Things []Thing

func connectDb() (*sql.DB, error) {
	connStr := fmt.Sprintf("host=%s port=%s user=%s password=%s dbname=%s sslmode=disable", dbHost, dbPort, dbUser, dbPassword, dbName)
	db, err := sql.Open("postgres", connStr)
	if err != nil {
		return nil, err
	}

	return db, nil
}

type DbQueryRow interface {
	QueryRow(query string, args ...interface{}) *sql.Row
}

func qa(db DbQueryRow, pgfunc string, args ...interface{}) (bool, []byte) {
	var argString []string
	for i, _ := range args {
		argString = append(argString, fmt.Sprintf("$%d", i+1))
	}

	sqlStatement := fmt.Sprintf("SELECT ok, js from %s(%s)", pgfunc, strings.Join(argString, ","))

	var ok sql.NullBool
	var js []byte

	err := db.QueryRow(sqlStatement, args...).Scan(&ok, &js)
	if err != nil {
		log.Println("failed to execute sql:", err.Error())
		return false, nil
	}

	return ok.Bool, js
}

func main() {

}
