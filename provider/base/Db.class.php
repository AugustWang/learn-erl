<?php

/**
 * @copyright iYu
 * @package iYu
 * @link jecelyin@gmail.com
 * @author jecelyin peng
 * 经修改
 * Mysql数据库连接驱动
 */
class Db
{

	//当前连接ID
	public $link_id = null;
	//表前缀
	public $prefix = '';
	//最后插入ID
	public $insert_id = 0;
	//查询次数
	public $count = 0;
	private static $instance = array();
	public static $tCouter = 0;

	public function __construct($cfg)
	{
		if ($cfg['pconnect']) $this->link_id = mysql_pconnect($cfg['host'], $cfg['user'], $cfg['pass']);
		else $this->link_id = mysql_connect($cfg['host'], $cfg['user'], $cfg['pass']);
		if (!$this->link_id) $this->_halt('无法连接到数据库主机：' . $cfg['host']);
		if (!mysql_select_db($cfg['dbname'], $this->link_id)) $this->_halt('无法打开数据库：' . $cfg['dbname']);
		if ($cfg['encode']) mysql_query("SET NAMES {$cfg['encode']}", $this->link_id);
		unset($cfg);
		return $this->link_id;
	}
	
	/**
     * 事务开始
     */
    public function beginTransaction(){
        if(self::$tCouter<0){
            throw new Exception("beginTransaction 没有严格配对");
        }else if(self::$tCouter == 0){
//            try{ 
                $this->query("begin");
//            }catch(Exception $e){ throw new PDOException("beginTransaction 出错：" . $e->getMessage());}
        }
        self::$tCouter++;
    }
    
    /**
	 * 事务提交
     */
    public function commit(){
		if(self::$tCouter>1){
		    self::$tCouter--;
		}else if(self::$tCouter==1){
		    self::$tCouter--;
//    	    try{ 
    	        $this->query("commit");
//    	    }catch(Exception $e){ throw new PDOException("commit 出错：" . $e->getMessage());}
		}else{
		    throw new PDOException("commit 没有严格配对");
		}
    }
    
	/**
	 * 事务回滚
     */
	public function rollback(){
	    if(self::$tCouter>1){
		    self::$tCouter--;
		    throw new PDOException("rollback 内嵌回滚");
		}else if(self::$tCouter==1){
		    self::$tCouter--;
//    	    try{ 
    	        $this->query("rollback");
//    	    }catch(Exception $e){ throw new PDOException("rollback 出错：" . $e->getMessage());}
		}else{
		    throw new PDOException("rollback 没有严格配对");
		}
	}

	/**
	 * 获取单例
	 */
	public static function getInstance($db = "database")
	{
		if (self::$instance[$db])
		{
			return self::$instance[$db];
		}

		$dsn = Config::getInstance()->get($db);
		try
		{
			self::$instance[$db] = new self($dsn);
		} catch (Exception $e)
		{
			throw new Exception('数据库连接失败');
		}

		return self::$instance[$db];
	}

	public function query($sql)
	{
		$sql = str_replace('@#@_', $this->prefix, $sql);
		$query_id = mysql_query($sql, $this->link_id);
		!$query_id && $this->_halt('查询失败：' . str_replace(array("\n", "\r"), '', $sql));
		$this->count++;
		$sql = trim($sql);
		//删除或更新时返回影响行数
		if (preg_match("/^(delete|update) /i", $sql)) return mysql_affected_rows($this->link_id);
		//插入或替换时返回最后影响的ID
		if (preg_match("/^(insert|replace) /i", $sql))
		{
			$this->insert_id = mysql_insert_id($this->link_id);
			return $this->insert_id;
		}
		return $query_id;
	}

	public function exec($sql)
	{
		$this->query($sql);
		return mysql_affected_rows($this->link_id);
	}

	/**
	 * 跟据数组中的数据返回一条插入语句
	 * (目前只考虑Mysql支持)
	 *
	 * @param string $table 表名
	 * @param array $data 数据
	 * @return string SQL语句
	 */
	public static function getInsertSql($table, $data, $addQuotes=True)
	{
		//peng：这里不应该再转义一次
		if($addQuotes)
		    $data = addQuotes($data);

		$col = array();
		$val = array();
		foreach ($data as $k => $v)
		{
			if (null === $v) continue;
			$col[] = $k;
			$val[] = $v;
		}
		return "insert into `{$table}`(`" . implode($col, '`, `') . "`) values('" . implode($val, "', '") . "')";
	}

	/**
	 * 跟据数组中的数据返回一条更新语句
	 * (目前只考虑Mysql支持)
	 *
	 * @param string $table 表名
	 * @param string|array $primaryKey 主键名，多主键时用数组传递
	 * @param array $data 数据
	 * @return false|string false或SQL语句
	 */
	public static function getUpdateSql($table, $primaryKey, $data, $addQuotes=True)
	{
		if($addQuotes)
		    $data = addQuotes($data);

		$w = array();
		if (is_array($primaryKey))
		{
			foreach ($primaryKey as $v)
			{
				$w[] = "`$v`='{$data[$v]}'";
				if (isset($data[$v]))
				{
					unset($data[$v]);
				} else
				{
					return false;
				}
			}
		} else if (isset($data[$primaryKey]))
		{
			$w[] = "`$primaryKey`='{$data[$primaryKey]}'";
			unset($data[$primaryKey]);
		} else
		{
			return false;
		}

		if (!$data) return false;

		$u = array();
		foreach ($data as $k => $v)
		{
			if (null === $v) continue;
			$u[] = "`{$k}`='{$v}'";
		}
		return "update `{$table}` set " . implode($u, ', ') . " where " . implode($w, ' and ');
	}

	/**
	 * 执行一个Limit查询
	 * 用法示例:
	 * $dbh = Db::getInstance();
	 * $sql = 'select * from user';
	 * $rs = $dbh->selectLimit($sql, 0, 10); //取出第0行开始的10条数据
	 * while($row = $rs->fetch()){
	 *     print_r($row);
	 * }
	 *
	 * @param string $sql SQL语句
	 * @param int $offset 偏移量
	 * @param int $num 要求返回的记录数
	 * @return Object 返回一个结果集句柄
	 */
	public function selectLimit($sql, $offset, $num)
	{
		$sql .= " limit $offset, $num";
		$rs = $this->query($sql);
		return $rs;
	}

	public function fetch_array($queryId, $result_type = MYSQL_ASSOC)
	{
		$result = mysql_fetch_array($queryId, $result_type);
		//is_array($result) && $result = array_map('trim', $result);
		//mysql_free_result($queryId);
		return $result;
	}

	public function getRow($query, $result_type = MYSQL_ASSOC)
	{
		return $this->fetch_array($this->query($query), $result_type);
	}

	public function getOne($query, $offset = 0)
	{
		$result = mysql_fetch_row($this->query($query));
		return $result === false ? false : $result[0];
	}

	public function getAll($query, $result_type = MYSQL_ASSOC)
	{
		$query_id = $this->query($query);
		$cacheArray = array();
		while ($result = $this->fetch_array($query_id, $result_type)) {
			$cacheArray[] = $result; //trim($result);
		}
		return $cacheArray;
	}

	public function insert_id()
	{
		if ($this->insert_id)
		{
			return $this->insert_id;
		} else
		{
			return mysql_insert_id($this->link_id);
		}
	}

	public function getSql($action, $table, $data, $where = array())
	{
		switch (strtolower($action))
		{
			case 'insert':
			case 'replace':
				$fields = array_keys($data);
				return strtoupper($action) . " INTO `$table` (`" . implode('`,`', $fields) . "`) VALUES ('" . implode("','", $data) . "')";
			case 'update':
				$sp = $s = $w = '';
				foreach ($data as $k => $v)
				{
					$s .= $sp . "`$k` = '{$v}'";
					$sp = ', ';
				}
				if ($where)
				{
					$sp = '';
					if (is_array($where)) foreach ($where as $k => $v)
						{
							$w .= $sp . "`$k` = '$v'";
							$sp = ' AND ';
						}
					else $w = $where;
				}
				return strtoupper($action) . " `{$table}` SET $s WHERE $w";
		}
	}

	public function insert($table, $data)
	{
		return $this->query($this->getSql('insert', $table, $data));
	}

	public function replace($table, $data)
	{
		return $this->query($this->getSql('replace', $table, $data));
	}

	/**
	 * @param $table 要更新的表名
	 * @param $data 要更新的数据的数组，其中数组的key对应字段名，值对应字段的值
	 * @param $where 更新对象的数组或字符串
	 */
	public function update($table, $data, $where)
	{
		return $this->query($this->getSql('update', $table, $data, $where));
	}

	public function affected_rows()
	{
		return mysql_affected_rows($this->link_id);
	}

	public function num_rows($sql)
	{
		return mysql_num_rows($this->query($sql));
	}

	private function _halt($msg)
	{
		if(!DEBUG)exit('invalid query.');
		$error = mysql_error($this->link_id);
		$errno = mysql_errno($this->link_id);
		$debug_info = debug_backtrace();
		$debug_info = array_reverse($debug_info);
		$err_html = '';
		$err_html .= "<b>Database error:</b><br /> $msg <br />";
		$err_html .= "<b>MySQL Error:</b><br />errno: {$errno} <br />error: {$error}<br /><br />";
		foreach ($debug_info as $v)
			if (isset($v['file'])) $err_html .= "<b>File:</b> {$v['file']} (Line: {$v['line']})<br />";
		echo "<pre>".$err_html."</pre>";
		exit();
	}

}
