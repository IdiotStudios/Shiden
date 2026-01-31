# filesystem library

## Usage:

Reading stuff  
the variable env will have the contents of .env  
```shiden
let env = fs read ./.env/

println(env)
```
Output:
API_KEY=abcdefghijklmnopqrstuvwxyz


Writing Stuff  
This overwrites the file
```shiden
fs write example.json({
  "example": true
})
```

Editing Stuff  
This will edit the file to have example be false  
It can also add things to the file
```shiden
fs edit example.json("example": false)
```

This is just examples, the actual library will be better