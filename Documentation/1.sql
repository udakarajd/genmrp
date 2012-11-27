# Tasks schema
 
# --- !Ups
CREATE TABLE company(
	compid varchar(40) NOT NULL,
	name   varchar(200) NOT NULL,
	password varchar(20) NOT NULL,
	description varchar(400),
	PRIMARY KEY(compid)	
);



CREATE TABLE part (
  compid varchar(40) NOT NULL,
  partid varchar(40) NOT NULL,
  partname varchar(255) NOT NULL,
  leadtime int(11) DEFAULT NULL,
  PRIMARY KEY (partid),
  FOREIGN KEY (compid) REFERENCES company(compid) 
);

alter table part add onhand int DEFAULT 0 after leadtime;

CREATE TABLE ordersnapshot(
	compid varchar(40) NOT NULL,
	partid varchar(40) NOT NULL,
	orderid int NOT NULL,
	scheduleid int NOT NULL,
	days int,
	quantity int,
	CONSTRAINT pk_orderid PRIMARY KEY (orderid,scheduleid),
	FOREIGN KEY(partid) REFERENCES part(partid),
    FOREIGN KEY(compid) REFERENCES company(compid)
	
);


alter table ordersnapshot add pordays int after quantity;
alter table ordersnapshot add porquantity int after pordays;

create view orders as
select compid,partid,orderid,scheduleid,days,quantity
from ordersnapshot order by days;


-- Create por is not a good idea. 
/*
CREATE TABLE  por(
	compid varchar(40) NOT NULL,
	partid varchar(40) NOT NULL,
	orderid int NOT NULL,
	scheduleid int NOT NULL,
	days int,
	quantity int,
	FOREIGN KEY(partid) REFERENCES part(partid),
    	FOREIGN KEY(compid) REFERENCES company(compid)	
)
*/
CREATE TABLE bom(
	compid varchar(40) NOT NULL,
	partid varchar(40) NOT NULL,
	childpartid  varchar(40) NOT NULL,
	quantity int,
    	CONSTRAINT bomid PRIMARY KEY(compid,partid,childpartid),
	FOREIGN KEY(compid) REFERENCES company(compid),
	FOREIGN KEY(partid) REFERENCES part(partid)
);





CREATE TABLE costfunction(
	compid varchar(40) NOT NULL,
	partid varchar(40) NOT NULL,
	funcid  varchar(40) NOT NULL,
	functxt varchar(400) NOT NULL, 
	CONSTRAINT funckey PRIMARY KEY(compid,partid,funcid),
	FOREIGN KEY(partid) REFERENCES part(partid),
        FOREIGN KEY(compid) REFERENCES part(compid)
	
);

CREATE TABLE genengine(
	compid varchar(40) NOT NULL,
	poolsize int DEFAULT 10,
	iterations int DEFAULT 10,	
	FOREIGN KEY(compid) REFERENCES company(compid)
)

ALTER TABLE genengine ADD maxorder int DEFAULT 200;




# --- !Downs
DROP TABLE costfunction;
DROP TABLE bom;
DROP TABLE ordersnapshot
DROP TABLE part;
DROP TABLE company;
