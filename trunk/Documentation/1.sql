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

CREATE TABLE ordersnapshot(
	compid varchar(40) NOT NULL,
	partid varchar(40) NOT NULL,
	orderid varchar(40) NOT NULL,
	scheduleid varchar(40) NOT NULL, 
	days int,
	quantity int,
	CONSTRAINT pk_orderid PRIMARY KEY (orderid,scheduleid),
	FOREIGN KEY(partid) REFERENCES part(partid),
    FOREIGN KEY(compid) REFERENCES company(compid)
	
);

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
	FOREIGN KEY(partid) REFERENCES part(partid),
    FOREIGN KEY(compid) REFERENCES part(compid)
	
);





# --- !Downs
DROP TABLE costfunction;
DROP TABLE bom;
DROP TABLE ordersnapshot
DROP TABLE part;
DROP TABLE company;
