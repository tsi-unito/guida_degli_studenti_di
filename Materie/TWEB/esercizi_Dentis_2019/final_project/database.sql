CREATE DATABASE servizio;
USE servizio;

CREATE TABLE Users(
	id int AUTO_INCREMENT,
    name varchar(40) NOT NULL,
	username varchar(40) UNIQUE NOT NULL,
	password varchar(64) NOT NULL,
	cucina BIT(1) NOT NULL,
	PRIMARY KEY(id));

INSERT INTO Users
VALUES
	(0,'administrator','admin','8c6976e5b5410415bde908bd4dee15dfb167a9c873fc4bb8a81f6f2ab448a918',1),
	(0,'testCucina','cucina','46b4eb8144194bab6b3f965c7e219258f71aed7475e005b8312e22cd5ee679d7',1),
	(0,'testSala','sala','d3cb0b473dc5daeacd5b4bf5147aad16af401da648ba206ede4a31ac4a4e1666',0);

CREATE TABLE Menu(
	id int NOT NULL,
	nome varchar(40) NOT NULL,
	categoria varchar(40) NOT NULL,
	PRIMARY KEY(id)
	);

INSERT INTO Menu
VALUES
	(0,'Hamburger','Hamburger'),
	(1,'Cheeseburgher','Hamburger'),
	(2,'Tirolese','Hamburger'),
	(3,'Skaburgher','Hamburger'),
	(4,'Crinass','Hamburger'),
	(5,'Diabolico','Hamburger'),
	(6,'Bigburgher','Hamburger'),
	(7,'Porcocane','Hamburger'),
	(8,'Pestifero','Hamburger'),
	(9,'Carnazza','Hamburger'),
	(10,'Vienna','Piadina'),
	(11,'Sturmtruppen','Piadina'),
	(12,'Taxy','Piadina'),
	(13,'Compromesso','Piadina'),
	(14,'Riviera','Piadina'),
	(15,'Gabibbo','Piadina'),
	(16,'Jazz','Piadina'),
	(17,'Gnari','Piadina'),
	(18,'Blues','Piadina'),
	(19,'Contadino','Piadina'),
	(20,'Porco','Piadina'),
	(21,'Maiala','Piadina'),
	(22,'Sangone','Piadina'),
	(23,'Vegeta','Piadina'),
	(24,'Flamenco','Piadina'),
	(25,'Cheese','Piadina'),
	(26,'Patate fritte','Piatti'),
	(27,'Patate Gorgonzola','Piatti'),
	(28,'Tagliere formaggi','Piatti'),
	(29,'Falso Bavarese','Piatti'),
	(30,'Alpegio','Piatti'),
	(31,'Sancrau','Piatti'),
	(32,'Nouvo Valsagone','Piatti'),
	(33,'Gambrinus','Piatti'),	
	(34,'Coazzino','Piatti'),
	(35,'Porcello','Piatti'),
	(36,'Polletto e Patate','Piatti'),
	(37,'Costine','Piatti'),
	(38,'Marinara','Pizze'),
	(39,'Margherita','Pizze'),
	(40,'Napoletana','Pizze'),
	(41,'Prosciutto','Pizze'),
	(42,'Diavola','Pizze'),
	(43,'Marsigliese','Pizze'),
	(44,'Piemontesa','Pizze'),
	(45,'Fumé','Pizze'),
	(46,'Vegetariana','Pizze'),
	(47,'Real Coazze','Pizze'),
	(48,'Classica','Pizze'),	
	(49,'Bianca','Pizze'),
	(50,'Real Coazze','Pizze'),
	(51,'Bionda','Birre'),
	(52,'Pils','Birre'),
	(53,'Doppio','Birre'),
	(54,'Rossa','Birre'),
	(55,'Brusatà','Birre'),
	(56,'Aleghetor','Birre'),
	(57,'Born','Birre'),
	(58,'Whyzen','Birre'),
	(59,'Wolf','Birre'),
	(60,'Estiva','Birre'),
	(61,'Avia','Birre'),
	(62,'Festassa','Birre');

CREATE TABLE Ordine(
	id int NOT NULL ,
	tavolo int NOT NULL,
	piattoId int NOT NULL,
	quantita int NOT NULL,
	PRIMARY KEY (id,piattoId),
	FOREIGN KEY (piattoId) REFERENCES Menu(id)
);
