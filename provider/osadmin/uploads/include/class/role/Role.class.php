<?php
if(!defined('ACCESS')) {exit('Access denied.');}

class Role extends RoleBase {
    //class Role extends Base {
    private static $table_name = 'role';

    private static $columns = 'role_id, role_content';


    public static function getTableName(){
        return self::$table_name;
    }

    public static function getRoles() {

        //�������ַ�ʽ�����Է���role��DB
        $db=self::__instance();
        //$db=self::__instance(ROLE_DB_ID);

        $sql="select ".self::$columns." from ".self::getTableName();
        $list = $db->query($sql)->fetchAll();
        if ($list) {
            return $list;
        }
        return array ();
    }
}
