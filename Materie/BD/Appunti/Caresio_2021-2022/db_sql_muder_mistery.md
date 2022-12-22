---
title: "Basi di dati - SQL Murder Mistery"
---

# Basi di dati - SQL Murder Mistery

>A crime has taken place and the detective needs your help. The detective gave you the crime scene report, but you somehow lost it. You vaguely remember that the crime was *murder* that occurred sometime on Jan.15, 2018 and  that it took place in SQL City. Start by retrieving the corresponding crime scene report from the police department’s database.

```sql
SELECT * FROM crime_scene_report WHERE type = 'murder' AND city = 'SQL City' AND date = '20180115';
```

- Security footage shows that there were 2 witnesses.
    - The first witness lives at the last house on "Northwestern Dr".
    - The second witness, named Annabel, lives somewhere on "Franklin Ave".

```sql
SELECT * FROM person WHERE address_street_name = 'Northwestern Dr' AND address_number = (SELECT MAX(address_number) FROM person);
SELECT * FROM person WHERE name LIKE 'Annabel%' AND address_street_name = 'Franklin Ave'
```

- Witnesses:
    - Morty Schaphiro (id: 14887, ssn: 111564949, license: 118009);
    - Annabel Miller (id: 16371, ssn: 318771143, license: 490173).

```sql
SELECT * FROM interview WHERE person_id = 14887;
SELECT * FROM interview WHERE person_id = 16371;
```

- Interview by Morty Schapiro:
    - I heard a gunshot and then saw a man run out.
    - He had a "Get Fit Now Gym" bag.
    - The membership number on the bag started with "48Z".
    - Only gold members have those bags.
    - The man got into a car with a plate that included "H42W".
- Interview by Annabel Miller:
    - I saw the murder happen, and I recognized the killer from my gym when I was working out last week on January the 9th.

```sql
SELECT name, person.id, ssn, person.license_id
FROM person JOIN drivers_license ON person.license_id = drivers_license.id
WHERE gender = 'male' AND plate_number LIKE '%H42W%';
```

- People with plate number containing "H42W":
    - Tushar Chandra (id: 51739, ssn: 137882671, license: 664760);
    - Jeremy Bowers (id: 67318, ssn: 871539279, license: 423327);

```sql
SELECT * FROM get_fit_now_member WHERE id LIKE '48Z%' AND membership_status = 'gold';
```

- Gold gym members with membership number starting with "48Z":
    - Joe Germuska (id: 28819, gym membership: 48Z7A);
    - Jeremy Bowers (id: 67318, gym membership: 48Z55).
- Both of previous results contains Jeremy Bowers.

```sql
SELECT * FROM get_fit_now_check_in WHERE check_in_date = '20180109' AND membership_id = '48Z55';
```

- Jeremy Bowers appears to be at the gym on January the 9th 2018.
    - Check-in date: 2018-01-09, Check-in time: 15.30, Check-out time: 17-00.

```sql
INSERT INTO solution VALUES (1,'Jeremy Bowers');
SELECT * FROM solution; -- To check the result
```

> Congrats, you found the murderer! But wait, there's more... If you think you're up for a challenge, try querying the interview transcript of the murderer to find the real villain behind this crime. If you feel especially confident in your SQL skills, try to complete this final step with no more than 2 queries.

```sql
SELECT interview.transcript
FROM person JOIN interview ON person.id = interview.person_id
WHERE person.name = 'Jeremy Bowers';
```

- I was hired by a woman with a lot of money.
    - I don't know her name but I know she's around 5'5" (65") or 5'7" (67").
    - She has red hair and she drives a Tesla Model S. 
    - I know that she attended the SQL Symphony Concert 3 times in December 2017.

```sql
SELECT person.name
FROM person JOIN drivers_license ON person.license_id = drivers_license.id
WHERE gender = 'female' AND car_make = 'Tesla' AND car_model = 'Model S' AND
      height >= 65 AND height <= 67 AND hair_color = 'red' AND
      person.id IN (
        SELECT person_id
        FROM facebook_event_checkin
        WHERE event_name LIKE '%SQL Symphony Concert%' AND date >= 20171201 AND date <= 20171231
        GROUP BY person_id
        HAVING COUNT(*) = 3);
```

- Result: Miranda Priestly.

```sql
INSERT INTO solution VALUES (1,'Miranda Priestly');
SELECT * FROM solution; -- To check the result
```

> Congrats, you found the brains behind the murder! Everyone in SQL City hails you as the greatest SQL detective of all time. Time to break out the champagne!
