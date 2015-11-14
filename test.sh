# search get request
curl 127.0.0.1:8080/search

printf "\n\nadd post request of contact with only email and phone (both valid)\n"
curl -H "Content-Type: application/json" -d "{\"email\":\"jraymond@wesleyan.edu\",\"phone\":\"1112223333\",\"attributes\":[]}" 127.0.0.1:8080/add

printf "\nadd post request of contact with additional attribute\n"
curl -H "Content-Type: application/json" -d "{\"email\":\"hcurry@penstate.edu\",\"phone\":\"2223334444\",\"attributes\":[[\"name\",\"haskell curry\"]]}" 127.0.0.1:8080/add

printf "\nadd post request of contact with additional attributes\n"
curl -H "Content-Type: application/json" -d "{\"email\":\"bobama@wesleyan.edu\",\"phone\":\"5556667777\",\"attributes\":[[\"name\",\"barack obama\"],[\"birthday\",\"May 6th 1965\"],[\"occupation\",\"president\"]]}" 127.0.0.1:8080/add

printf "\nadd post request with invalid email\n"
curl -H "Content-Type: application/json" -d "{\"email\":\"bademail.@example.com\",\"phone\":\"1112223333\",\"attributes\":[[\"name\",\"invalid email\"]]}" 127.0.0.1:8080/add

printf "\nadd post request with invalid phone\n"
curl -H "Content-Type: application/json" -d "{\"email\":\"okemail@example.com\",\"phone\":\"111111\",\"attributes\":[[\"name\",\"invalid phone \"]]}" 127.0.0.1:8080/add

printf "\nadd post request with invalid email and invalid phone\n"
curl -H "Content-Type: application/json" -d "{\"email\":\"bademail.@example.com\",\"phone\":\"1111\",\"attributes\":[[\"name\",\"invalid both\"]]}" 127.0.0.1:8080/add

printf "\nsearch\n"
curl 127.0.0.1:8080/search

printf "\nadding more contacts\n"
curl -H "Content-Type: application/json" -d "{\"email\":\"temp0@temp.temp\",\"phone\":\"0776665555\",\"attributes\":[[\"name\",\"temp0\"]]}" 127.0.0.1:8080/add

printf "\n\n"
curl -H "Content-Type: application/json" -d "{\"email\":\"temp1@temp.temp\",\"phone\":\"1776665555\",\"attributes\":[[\"name\",\"temp1\"]]}" 127.0.0.1:8080/add

printf "\n\n"
curl -H "Content-Type: application/json" -d "{\"email\":\"temp2@temp.temp\",\"phone\":\"2776665555\",\"attributes\":[[\"name\",\"temp2\"]]}" 127.0.0.1:8080/add

printf "\nsingle delete post request example\n"
curl -H "Content-Type: application/json" -d "[4]" 127.0.0.1:8080/delete

printf "\nmultiple delete post request example\n"
curl -H "Content-Type: application/json" -d "[5,6]" 127.0.0.1:8080/delete

printf "\nsearch\n"
curl 127.0.0.1:8080/search

printf "\nupdate contact with new attribute\n"
curl -H "Content-Type: application/json" -d "[1,[[\"updated\",\"true\"]]]" 127.0.0.1:8080/update

printf "\nupdate contact that does not exist with new attribute (nothing happens)\n"
curl -H "Content-Type: application/json" -d "[99,[[\"updated\",\"true\"]]]" 127.0.0.1:8080/update

printf "\nupdate contact email\n"
curl -H "Content-Type: application/json" -d "[1,[[\"email\",\"update@email.com\"]]]" 127.0.0.1:8080/update

printf "\nupdate contact phone \n"
curl -H "Content-Type: application/json" -d "[1,[[\"phone\",\"3332221111\"]]]" 127.0.0.1:8080/update

printf "\nupdate contact invalid phone\n"
curl -H "Content-Type: application/json" -d "[1,[[\"phone\",\"0\"]]]" 127.0.0.1:8080/update

printf "\nupdate contact invalid email\n"
curl -H "Content-Type: application/json" -d "[1,[[\"email\",\"wat\"]]]" 127.0.0.1:8080/update

printf "\nupdate contact multiple attributes\n"
curl -H "Content-Type: application/json" -d "[1,[[\"updated\",\"truetrue\"],[\"birthday\",\"the ides of march\"]]]" 127.0.0.1:8080/update

printf "\nsearch\n"
curl 127.0.0.1:8080/search

printf "\n"
