-- MySQL dump 10.13  Distrib 5.7.17, for macos10.12 (x86_64)
--
-- Host: localhost    Database: vuln
-- ------------------------------------------------------
-- Server version	5.7.24-0ubuntu0.18.04.1

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `states`
--

DROP TABLE IF EXISTS `states`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `states` (
  `states_id` int(11) NOT NULL AUTO_INCREMENT,
  `states_name` varchar(45) NOT NULL,
  `states_description` varchar(45) NOT NULL,
  PRIMARY KEY (`states_id`),
  UNIQUE KEY `states_id_UNIQUE` (`states_id`)
) ENGINE=InnoDB AUTO_INCREMENT=16 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `states`
--

LOCK TABLES `states` WRITE;
/*!40000 ALTER TABLE `states` DISABLE KEYS */;
INSERT INTO `states` VALUES (1,'discover_host','this is discover host'),(2,'open_ports','this is open ports'),(3,'ftp_server','this is ftp server'),(4,'user_list','this is user list'),(5,'server_access_root','root server access'),(6,'ssh_server','this is ssh server'),(7,'passwords','this is passwords'),(8,'server_access_user','user server access'),(9,'hashed_passwords','this is hashed password'),(10,'port_80','http port'),(11,'web_access','access for web'),(12,'login_page','this is login page'),(13,'web_admin_access','admin web access'),(14,'database_queries','this is database queries'),(15,'bypass_auth','bypass authentication');
/*!40000 ALTER TABLE `states` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `vuln`
--

DROP TABLE IF EXISTS `vuln`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `vuln` (
  `vuln_id` int(11) NOT NULL AUTO_INCREMENT,
  `vuln_name` varchar(45) NOT NULL,
  `vuln_description` varchar(55) NOT NULL,
  `vuln_config` varchar(200) DEFAULT NULL,
  PRIMARY KEY (`vuln_id`),
  UNIQUE KEY `vuln_ID_UNIQUE` (`vuln_id`)
) ENGINE=InnoDB AUTO_INCREMENT=22 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `vuln`
--

LOCK TABLES `vuln` WRITE;
/*!40000 ALTER TABLE `vuln` DISABLE KEYS */;
INSERT INTO `vuln` VALUES (1,'host-discovery','this is host discovery',NULL),(2,'port-scanning','port scann',NULL),(3,'open-ftp','this is ftp','ftp-[]]'),(4,'directory-traversal','this is directory traversal',NULL),(5,'login-root(brute-force)','brute force for root','[ftp-[root_password-(only, [\"\'$6$pcWmIiBJypefToKL$OgKL0uQx43wxEf9RkeaOFGkyRJvmJzlj0Farr6to0lLmkMiiG7PPKKvUKky1W0b2LBx93p/EVF2dss79E18J4/\'\"])]]'),(6,'open-ssh','this is ssh port22',NULL),(7,'login-root(brute-force)','this is brute force (login-root)','[ssh-[root_password-(only, [\"\'$6$ugnivWA.s5k8bbgi$0zQz2ILZIFmKpCNr5RvVzFn6pYcY7IGZTYr5A3kLRMSrEuOuXrTmw2uhZUU8NAaAxA1Ma9wWHMh2PCT4jjYj20\'\"])]]'),(8,'login-root(credentials)','credentials for root','[ssh-[root_password-(only, [\"\'$6$ugnivWA.s5k8bbgi$0zQz2ILZIFmKpCNr5RvVzFn6pYcY7IGZTYr5A3kLRMSrEuOuXrTmw2uhZUU8NAaAxA1Ma9wWHMh2PCT4jjYj20\'\"])]]'),(9,'login-user(brute-force)','brute force (login user)','[ssh-[users-(exists, [example]), passwords-(exists,[\"\'$6$zXfK0cjCjTOYGk/t$U1DcfcpU2FQnaT5f/RfRHzRpq.va/d.YuDj7HcNW.B87yhaJgPtuik4CWlWa7drm9oGhZ941OXAtSF7CzXIgP1\'\"])]]'),(10,'login-user(credentials)','credentials login user','[ssh-[users-(exists, [example]), passwords-(exists, [\"\'$6$zXfK0cjCjTOYGk/t$U1DcfcpU2FQnaT5f/RfRHzRpq.va/d.YuDj7HcNW.B87yhaJgPtuik4CWlWa7drm9oGhZ941OXAtSF7CzXIgP1\'\"])]]'),(11,'crack-hashes','this is crack hashes',NULL),(12,'login-root(credentials)','credentials for root (different config)','[ssh-[root_password-(only, [\"\'$6$XFCpeL8iXtL4MYOx$aDRdDFz941M3tsOHmQWEHicpzotdYQXXY3/eagI5uIRii.moOaKQPv93z6e6uC.9p44PjgVniy9IH1bA.Q0/W.\'\"])]]'),(13,'open-web','this is open web',NULL),(14,'web-access','this is web access','[apache-[]]'),(15,'login-admin(brute-force)','this is login admin brute force','[php-[git_repo-(exists, [\'brute-force\']), repo_folder-(exists, [\'alpaca_bruteforce\'])], mysql-[db-(exists, [\'alpaca_bruteforce\']), sql_files-(exists, [\'accounts.sql\'])]]'),(16,'login-admin(credentials)','login admin credentials','[php-[git_repo-(exists, [\'alpaca_bruteforce\']), repo_folder-(exists, [\'alpaca_bruteforce\'])], mysql-[db-(exists, [\'alpaca_bruteforce\']), sql_files-(exists, [\'accounts.sql\'])]]'),(17,'sql-injection','this is sql injection','[php-[git_repo-(exists, [\'https://KimAChen@bitbucket.org/KimAChen/alpaca_sqli.git\']), repo_folder-(exists, [\'alpaca_sqli\'])], mysql-[db-(exists, [\'alpaca_sqli\']), sql_files-(exists, [\'users.sql\'])]]'),(18,'db-query-users','query for user','[mysql-[users-(exists, [\'alpaca_sqli\']), passwords-(exists, [\'password\'])]]'),(19,'login-root(credentials)','credentials login root (has different config)','[ssh-[root_password-(only, [\"\'$6$Rt0CznBsC/VVsFNT$zIIU0FjskC3QTdWFWro2x1xaQ/BVLJ3d0UKtkw07XPGCQQVVRj5B8kvYe3CSJV.mChqIZ6jKvg.q0CdXuHSzb.\'\"])]]'),(20,'directory-traversal','this is directory traversal','[php-[git_repo-(only, [\'alpaca_traversal\']), repo_folder-(exists, [\'alpaca_traversal\'])], mysql-[db-(exists, [\'alpaca_traversal\']), sql_files-(exists, [\'accounts.sql\'])]]'),(21,'bypass-authentication(admin)','this is bypass authentication for admin',NULL);
/*!40000 ALTER TABLE `vuln` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `vuln_post`
--

DROP TABLE IF EXISTS `vuln_post`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `vuln_post` (
  `howManyPost` int(11) NOT NULL AUTO_INCREMENT,
  `vuln_id` int(11) NOT NULL,
  `states_id` int(11) NOT NULL,
  PRIMARY KEY (`howManyPost`),
  UNIQUE KEY `howManyPost_UNIQUE` (`howManyPost`)
) ENGINE=InnoDB AUTO_INCREMENT=23 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `vuln_post`
--

LOCK TABLES `vuln_post` WRITE;
/*!40000 ALTER TABLE `vuln_post` DISABLE KEYS */;
INSERT INTO `vuln_post` VALUES (1,1,1),(2,2,2),(3,3,3),(4,4,4),(5,5,5),(6,6,6),(7,7,5),(8,8,5),(9,9,8),(10,10,8),(11,11,7),(12,12,5),(13,13,10),(14,14,11),(15,15,13),(16,16,13),(17,17,14),(18,18,4),(19,18,9),(20,19,5),(21,20,15),(22,21,13);
/*!40000 ALTER TABLE `vuln_post` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `vuln_pre`
--

DROP TABLE IF EXISTS `vuln_pre`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `vuln_pre` (
  `HowManyPre` int(11) NOT NULL AUTO_INCREMENT,
  `vuln_id` int(11) NOT NULL,
  `states_id` int(11) DEFAULT NULL,
  PRIMARY KEY (`HowManyPre`),
  UNIQUE KEY `id_UNIQUE` (`HowManyPre`)
) ENGINE=InnoDB AUTO_INCREMENT=30 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `vuln_pre`
--

LOCK TABLES `vuln_pre` WRITE;
/*!40000 ALTER TABLE `vuln_pre` DISABLE KEYS */;
INSERT INTO `vuln_pre` VALUES (1,1,NULL),(2,2,1),(3,3,2),(4,4,3),(5,5,3),(6,6,2),(7,7,6),(8,8,7),(9,8,6),(10,9,6),(11,9,4),(12,10,6),(13,10,4),(14,10,7),(15,11,9),(16,12,7),(17,12,6),(18,13,2),(19,14,10),(20,15,11),(21,15,12),(22,16,11),(23,16,12),(24,17,12),(25,18,14),(26,19,7),(27,19,6),(28,20,11),(29,21,15);
/*!40000 ALTER TABLE `vuln_pre` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2018-11-08 14:07:27
