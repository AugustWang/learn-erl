<?php
/*-----------------------------------------------------+
 * 系统日志记录器
 * @author erlang6@qq.com
 +-----------------------------------------------------*/
class Logger{
    /**
     * 记录日志
     *
     * @param string $msg 日志内容
     * @param int $priority 级别
     */
    private static function log($msg, $priority){
        $data= array(
            'priority' => $priority,
            'msg' => $msg,
            'ip' => clientIp(),
            'ts' => time(),
        );
        $sql = Db::getInsertSql('log_system', $data);
        Db::getInstance()->exec($sql);
    }

    /**
     * 记录普通信息
     * @param string $msg 内容
     */
    public static function info($msg){
        self::log($msg, 0);
    }

    /**
     * 记录警告信息
     * @param string $msg 内容
     */
    public static function warning($msg){
        self::log($msg, 1);
    }

    /**
     * 记录错误信息
     * @param string $msg 内容
     */
    public static function error($msg){
        self::log($msg, 2);
    }
}
